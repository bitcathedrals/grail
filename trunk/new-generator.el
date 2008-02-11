;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright (C) 2008 Mike Mattie
;; Description: Recursive Descent parser compiler with a lisp macro
;;              interface.
;;----------------------------------------------------------------------

;; ->Features

;; 0. Parser construction is integrated directly into the lisp environment
;;    with macro expansion of the grammar into a parsing function.

;; 1. PEG like extensible grammar.

;; 2. The AST generated corresponds strictly to the parse tree only by
;;    default. The AST can be generated in tokens, and transformed in
;;    productions.

;; ->TODO

;;   -> Phase 1: correctness

;; 0. new parser generator merged in, and debugged.

;; 2. replace the generated cond tables in the grammar interpreter with hash tables

;; 1. All of the PEG predicates (missing not)

;; 2. re-document after all the code churn.
;;    need to C-h f functions in various context to see how the DocStrings look +
;;    read the DocString Style Guide.

;; 3. Lazy binding for in-grammar right recursion.

;; 3. Canonical tree walk implemented as parser-ast-node. a real pre-requisite to
;;    sane user implementation of transforms.

;; 4. commented out message string in compile needs to be a debugging
;;    option

;; 5. implement a error recovery routine. This involves marking tokens as being
;;    sync tokens, and generating a recovery function that scans for those tokens.

;;    -> Phase 2: optimization

;; 1. Implement packrat backtrack optimization.

;;    better idea. When a backtrack occurs a nil will be returned to
;;    the parse-ast-descend predicate. Normally the parse tree constructed
;;    so far would be discarded. Instead filter the terminals out, and
;;    keep them in a table.

;; 2. make left recursion work.

;; ->Terminology

;; My reference for parsing terminology is the Dragon Book:

;; Compilers
;; Principles,Techniques,and Tools
;; Alfred V.Aho, Ravi Sethi, Jeffrey D.Ullman
;; 1986, Addison Wesley

(require 'cl)
(require 'mattie-elisp) ;; USES define-error make-anon-func list-filter-nil

(define-error parser-compile-error   "parser error")
(define-error parser-syntactic-error "syntactic error" parser-compile-error)
(define-error parser-semantic-error  "semantic error" parser-compile-error)

;;----------------------------------------------------------------------
;; Backtracking.
;;----------------------------------------------------------------------

;; parser-position

;; The position of the parser in the input stream is maintained as a
;; stack of indexes into the buffer. As matching progresses the top of
;; the stack (car) is updated. The push and pop operations copy a
;; position to a next or previous stack position. backtrack discards
;; the top of the stack.

(defun parser-pos ()
  "Return the current position of the parser in the buffer"
  (car parser-position))

(defun parser-push ()
  "Copy the parser position to a new stack level so the parser can backtrack when necessary."
  (push (parser-pos) parser-position))

(defun parser-pop ()
  "Copy the parser position to a previous stack level When the possibility of a backtrack
   has been eliminated by matching."
  (let
    ((current (pop parser-position)))
    (setcar parser-position current) ))

(defun parser-backtrack ()
  "Restore the previous parser position by discarding the top of the parser-position stack.
   Always returns nil so it can be used as a failure Match Result."
  (pop parser-position)
  (goto-char (parser-pos))
  nil)

(defun parser-advance ( consumed )
  "Advance the input position of the parser to the next un-matched character."

  (if (> consumed 0)
    (lexical-let
      ((pos (+ consumed (parser-pos))))
      (progn
        (setcar parser-position pos)
        (goto-char pos))) ))

(defun parser-consumed ()
  "The number of input characters consumed by the token's match in the input."
  (- (match-end 0) (match-beginning 0)))

;;----------------------------------------------------------------------
;; compiler diagnostics
;;----------------------------------------------------------------------

;; construct meaningful compiler error messages in the
;; "expected: foo got: bar" form.

(defun parser-expr-diagnostic ( form )
  (format "type(%s) %s" (symbol-name (type-of form)) (pp (eval form))))

(defmacro parser-diagnostic ( form from expected )
  "syntax: (parser-diagnostic form from expected)

   Where form is the expr received, from is the component issuing the diagnostic,
   and expected is a message describing what the component expected"
  `(concat (format "[%s] expected: " ,from)  ,expected " not: " ,(parser-expr-diagnostic form)))

;;----------------------------------------------------------------------
;; Parser Tracing
;;----------------------------------------------------------------------

;; A tracing facility that can be selectively turned on and off for
;; productions. When tracing is turned on the result of all matches
;; attempted are printed to a buffer, or the Message buffer.

;; A report of how the compiled parser matched the input stream is
;; vital to developing a working grammar.

(defun parser-trace-message ( format &rest args )
  "Prints to a trace buffer instead of the Message buffer."
  (with-current-buffer parser-trace-buffer
    (goto-char (point-max))
    (insert (apply 'format format args))))

(defun parser-trace-match ( match-func match-result )
  "Trace the current match if the parser-trace-flag is bound to t"
  (if (and (boundp 'parser-trace-flag) (eq t parser-trace-flag))
    (funcall
      (if (boundp 'parser-trace-buffer)
        'parser-trace-message
        'message)

      "%s at: %d match: %s"
      (symbol-name match-func)
      (parser-pos)
      (pp-to-string match-result)) ))

(defun parser-trace-p ( production )
  "Given a Match Function determine if parser-trace-flag should
   be set. The parser-trace list is scanned for a symbol match.
   The return value is a cons of a boolean indicating whether to
   set the flag, and the value of the flag.

   The parser-trace list is created by the macro parser-trace-list
   in utilities."

  (catch 'abort
    (unless (and (boundp 'parser-trace) (listp parser-trace)) (throw 'abort nil))

    (lexical-let
      ((toggle (apply 'or (mapcar (lambda ( trace-on )
                                    ;; eq comparison of symbols does not work. A string
                                    ;; comparison is used for matching.

                                    ;; FIXME: symbol-value may remove this wart.
                                    (if (equal (symbol-name production) (car trace-on))
                                      (cdr trace-on)))
                            parser-trace) )))
      ;; a cons cell is returned so that a false value for the trace flag can be returned
      ;; without negating the truth value of the predicate itself.
      (if toggle
        (cons t toggle)
        (cons nil nil) )) ))

(defmacro parser-trace-on ( production &rest code )
  "parser-trace-on takes production and a code block code. If the production
   is on the parser-trace list a parser-trace-flag dynamically scoped is
   bound to the boolean toggle for tracing that production."

  (declare (debug symbolp body))

  ;; Using the dynamic scoping of let during the execution of the
  ;; compiled parser to scope parser-trace-flag gives tracing behavior
  ;; that precisely matches the execution of the parser.

  `(lexical-let*
    ((code-func (lambda () ,@code))
      (trace-p (parser-trace-p ,production))
      (trace-toggle (cdr trace-p)) )

     (if (and
           (car trace-p)

           ;; This expression attempts to minimize duplicate binding
           ;; of parser-trace-flag. If there are flaws in the tracing
           ;; behavior stemming from this expression it should be
           ;; removed entirely.
           (or
             (not (boundp 'parser-trace-flag))
             (not (eq parser-trace-flag trace-toggle))))
       (let
         ((parser-trace-flag trace-toggle))
         (funcall code-func))

       (funcall code-func)) ))

;;----------------------------------------------------------------------
;; Match Result
;;----------------------------------------------------------------------

;; The parser functions have a standard structure for returning the
;; result of a Match Function.

;; nil | (parser . AST ).

;; nil indicates failure to match.

;; A successful match has a parser and AST parts.

;; The parser part is either a count of input characters consumed
;; for a terminal or t|nil for non-terminals. When the parser
;; position is advanced the count is replaced with t.

;; The AST part is created by a token action handler or a combination
;; operator like and.

;; terminal     ( match-symbol . ( start . end ) | "user value")
;; non-terminal ( match-symbol . "a list/tree of AST" )

(defun parser-make-production-match ( tree )
  "Make a matched non-terminal Match Result."
  (cons t tree))

(defun parser-make-token-match ( data )
  "Make a matched terminal Match Result."
  (cons (parser-consumed) data))

(defun parser-make-logical-match ()
  "Make a logical match true."
  (cons t nil))

(defun parser-match-logic ( match-result )
  "Return the data of a Match Result."
  (car match-result))

(defun parser-match-data ( match-result )
  "Return the data of a Match Result."
  (cdr match-result))

(defun parser-match-p ( match-result )
  "return true if the logic is a match result"
  (and match-result (parser-match-logic match-result)))

;;----------------------------------------------------------------------
;; AST tree constructors
;;----------------------------------------------------------------------

;; AST is a tree or list where the first element is an identity symbol
;; with the parser-ast property production. subsequent elements are
;; either sub-trees identifiable with the production property or
;; tokens which are arbitrarily structured data cons to an identity
;; symbol.

(defun parser-make-ast ( foo )
  "Create an ast tree with a null identity."
  (lexical-let
    ((ast (cons 'null  nil)))
    (put ast 'parser-ast 'production)
    ast))

(defun parser-ast-p ( foo )
  (eq 'production (get (car foo) 'parser-ast)))

(defun parser-ast-merge-node ( node )
  "parser-ast-merge-node NODE

Merging AST nodes A and B is a union of A and B implemented as
list B appended to list A. The first element of list B is dropped
so that the merged node retains the identify of A. The complexity
of the merge is linear for n elements of B since the tail of A is
dynamically scoped.

the A node is dynamically scoped as the tail while the B node is
supplied as the single argument NODE."

  (lexical-let
    ((children (cdr node)))

    (setcdr parse-tree children)
    (setq parse-tree (do ((x chilren))
                       ((null (cdr x)) x)
                       (setq x (cdr x))) )))

(defun parser-ast-add-node ( node )
  "parser-ast-add-node NODE

Adding a node X to ast Y simply makes X an element of Y. The
structure if X is opaque to this operation unlike merging making
it suitable for injecting arbitrary data into AST.

the A node is dynamically scoped as the tail while the X node is
supplied as the single argument NODE."
  (setq parse-tree (setcdr parse-tree (cons node nil))))

(defun parser-consume-match ( match-result )
  "Consume any unconsumed AST in Match Result. The AST
   is cleared from the Match Result so that the AST is
   only modified once. The logical value of the Match
   Result is preserved."

  (catch 'consumed-match
    (lexical-let
      ((match-status (parser-match-logic match-result))
       (match-data   (parser-match-data match-result)))

      (if (numberp match-status)
        (progn
          (parser-advance match-status)

          (unless (and (symbolp match-data) (null match-data))
            (parser-ast-add-node match-data))

          (throw 'consumed-match (parser-make-logical-match)))

        ;; The alternative to a token is a possible un-consumed
        ;; production. Consumption occurs regardless of the logical
        ;; sense of the Match Result. This keeps logic and AST
        ;; orthogonal.

        ;; It is a trivial change to make consumption conditional if
        ;; the orthogonality is found to be too non-intuitive, but I
        ;; doubt this orthogonality will be visible at the semantics
        ;; level unless the user is deliberately introducing it.

        (when match-data
          (if (parser-ast-p match-data)
            (parser-ast-merge-node match-data)
            (parser-ast-add-node match-data))

          (throw 'consumed-match
            (if match-status
              (parser-make-logical-match)
              (cons nil nil)))))

      match-result)))

;;----------------------------------------------------------------------
;; Parser Predicates
;;----------------------------------------------------------------------

;; Parser predicates are the lowest level primitives. They can assume
;; that a AST tree tail has been scoped. Delayed evaluation allows
;; them to implement sequence logic and repetition which are easier
;; when the parser function is taken as an argument instead of
;; a Match Result.

(defun parser-predicate-or ( &rest match-list )
  "Combine Match Functions by or ; the first successful match is returned.
   nil is returned if no matches are found"
  (catch 'match
    (dolist (match-func match-list)
      (parser-trace-on match-func

        (catch 'parser-match-fail
          (lexical-let ((match-result (funcall match-func)))

            (parser-trace-match match-func match-result)
            (throw 'match match-result)) )))
    nil))

(defun parser-predicate-and ( &rest match-list )
  (dolist (match-func match-list (parser-make-logical-match))
    (parser-trace-on match-func
      (lexical-let ((match-result (funcall match-func)))
        (parser-trace-match match-func match-result)

        (unless (parser-match-p match-result)
          (throw 'parser-match-fail nil))

        (parser-consume-match match-result))) ))

(defun parser-predicate-greedy ( match-func )
  "A positive closure predicate that applies the given
   Match Function as many times as it Matches and returns true
   if one or more matches was made."
  (lexical-let ((matched-once nil))

    (catch 'parser-match-fail
      (do ((production (funcall match-func) (funcall match-func)))
          ((progn
             (parser-consume-match production)
             (setq matched-once t)
             nil)) ))
    (if matched-once (parser-make-logical-match)) ))

;;----------------------------------------------------------------------
;; Parser Function Generator.
;;----------------------------------------------------------------------

;; The Parser Function Generator generates the Match Functions from a
;; specification of parser primitives or semantics.

;; Parser primitives are the low level functions of a parser such as
;; back-tracking, positive closure, and matching logic.

;; Defining a useful set of orthogonal primitives, conditional
;; branching, and user defined hooks allows parsing to be extended in
;; the grammar while keeping the parser code generated instead of hand
;; written.

;; Implementation Notes:

;; -> Entry point

;; parser-function-generate is the entry point for generating a Parser
;; Function from a validated semantics table. All of the parser-gen-*
;; functions are parser-function-generate internals.

;; -> Semantics Table

;; The parser-function-semantics table is the data structure that
;; glues these generator functions together. It is a set of flags and
;; code fragments.

;; The validate part ensures that the semantics table is complete
;; including dependencies such as normally invisible generation
;; flags implied by the combination of semantics.

;; The generate part fills the semantics table with the necessary code
;; fragments based on the flags and interpolates the fragments into a
;; template.

(setq parser-function-semantics
  (make-closure

    (gen-sexp           nil) ;; generate sexp form when t, more generalized entry
                             ;; point may be the right thing.

    ;; Match Phase
    (gen-sequence       nil)  ;; a set of Match functions that imply gen-predicate
                              ;; as a relational operator.
    (gen-predicate      nil)  ;; a single Match Function or a relational operator.
    (gen-closure        nil)  ;; take the match call with delayed evaluation.
    (gen-trap           nil)  ;; internal, always turned on when needed. trap
                              ;; parser-match-fail

    ;; Parser Effects
    (gen-input-branch   nil)  ;; do input effects branch ?

    (gen-logic-branch   nil)  ;; default to return the logical value.
                              ;; of the function instead of the match.

    (gen-ast-discard    nil)  ;; discard is mutually exclusive with the set below.
                              ;; discard the ast tree.

    (gen-ast-transform  nil)  ;; user defined AST transform.
    (gen-ast-node       nil)  ;; Create named AST trees that are attached instead
                              ;; of merged.
    (gen-ast-branch     nil)  ;; do ast effects branch ?

    (gen-logic-operator nil)  ;; should be set to the logical operator function.

    ;; Return value

    ;; function | match
    (gen-return        'function)  ;; which logical result to return.

    ;; Validation and Generation Internals

    (eff-input          nil)
    (eff-ast            nil)
    (eff-logic          nil)

    (gen-branch         nil)  ;; do we have branching ?


    (gen-ast-value      nil)  ;; the result of a AST effect that is not a node.
                              ;; will be combined with the logical result to
                              ;; keep ast/logic orthogonal.

    (gen-lexical-scope  nil)  ;; list of (name value)... lists
    (gen-dynamic-scope  nil)

    ;; before/after hooks.
    (gen-match-before   nil)
    (gen-match-after    nil)

    (gen-func-rvalue    nil)

    ;; branching on match results.

    (gen-match-effects  nil)
    (gen-match-rvalue   nil)

    (gen-fail-effects   nil)
    (gen-fail-rvalue    nil)))

(defun parser-gen-lambda ( &rest generated )
  "parser-gen-lambda inserts a list of sexp into a quoted lambda form"
  `(lambda ()
     ,@(apply 'append (list-filter-nil generated))))

(defun parser-gen-match-call ()
  "The match call of a parser function includes the closure, predicate,
   and sequence. Evaluation produces the logical Match Result of the
   function after parser-consume-match strips any AST.

   inputs: gen-predicate, gen-sequence, gen-closure"
  `(parser-consume-match
     ,(lexical-let
       ((predicate nil))

       (if gen-sequence
         (progn
           (setq predicate `(apply ',gen-predicate '(,@gen-sequence)) )
           (if gen-closure
             `(funcall ',gen-closure ,(parser-gen-lambda predicate))
             predicate))

         (if gen-closure
           `(funcall ',gen-closure ',gen-predicate)
           `(funcall ',gen-predicate) )))))

(defun parser-gen-dynamic-scope ( generated )
  "parser-gen-dynamic-scope interpolates the sexp generated
   into a dynamic scope with the bindings of gen-dynamic-scope
   from the semantics table.

   inputs: gen-dynamic-scope"
  (if gen-dynamic-scope
    `(let
       ,gen-dynamic-scope
       ,generated)
  generated))

(defun parser-gen-match-phase ( generated )
  "The match phase of a Parser Function consists of the match
   call inside a dynamic scope and an optional trap of
   parser-match-fail when there is branching in the Parser
   Function.

   inputs: gen-trap"
  (lexical-let
    ((transform (parser-gen-dynamic-scope generated)))

    (if gen-trap
      `(catch 'parser-match-fail
         ,transform)
      transform)))

(defun parser-gen-with-effects ( effects rvalue )
  "parser-gen-with-effects EFFECTS RVALUE

   Create a return value with an optional list of side-effects.

   the second parameter RVALUE is always returned. EFFECTS
   can be a list or nil."
  (if effects
    `(progn
       ,@effects
       ,rvalue)
    (if rvalue
      `,rvalue)))

(defun parser-gen-input-effects ()
  "Create the input effects of the Parser Function.

   input: eff-input, gen-branch, gen-input-branch
   modifies: gen-match-before, gen-match-effects, gen-match-after"
  (catch 'done
    (unless eff-input (throw 'done nil))

    (push `(parser-push) gen-match-before)

    (if (and gen-branch gen-input-branch)
      (progn
        (push `(parser-pop) gen-match-effects)
        (push `(parser-backtrack) gen-fail-effects))
      (push `(parser-pop) gen-match-after)) ))

;; I think this design for AST transform is fucked up. That function will need
;; a way to determine if it's a token or a list it has been passed, which only
;; exists in the head symbol potentially.

(defun parser-gen-attach-transform ()
  "parser-gen-ast-effects auxiliary function."
  (if gen-ast-transform
    `(cons (car ast-root) (funcall ',gen-ast-transform (parser-match-data ast-root)))
    ;; return AST root because this function is only called right before
    ;; parser-ast-add-node which does not assume a Match Result argument,
    ;; just AST.
    'ast-root))

(defun parser-gen-ast-effects ()
  "Generate any AST effects for the parser function.

   The most basic AST effect is creating a AST node or list which
   consists of a lexically scoped head and a dynamically scoped
   tail.

   The flag eff-ast alone will generate an AST node.
   The remaining set of AST effects is either a branch-able
   gen-ast-discard or one of the effects below.

   gen-ast-node stub.

   gen-ast-transform stub.

   inputs: eff-ast, gen-ast-discard, gen-ast-node, gen-branch,
           gen-ast-branch, gen-ast-transform

   outputs: gen-ast-value, gen-lexical-scope, gen-dynamic-scope,
            gen-match-after,gen-match-effects"

  (catch 'done
    (unless eff-ast (throw 'done nil))

    ;; Initialize the head of the AST in the lexical scope.
    (push `(ast-root ,(if gen-ast-node
                        `(cons ',gen-ast-node nil)
                        `(parser-make-ast))) gen-lexical-scope)

    ;; Initialize the tail pointer in the dynamic scope.
    (push `(parse-tree ast-root) gen-dynamic-scope)

    (if gen-ast-discard
      (if gen-branch
        (push `(setq ast-root nil) gen-match-after)
        (setq gen-ast-value 'nil))

      (progn
        ;; a conditional selects the highest level of complexity first as
        ;; the entry point for generating the code.

        (cond

          ;; 1: Node: must be immediately attached and gen-ast-value left
          ;;          nil. Otherwise consumption would merge a node
          ;;          instead of adding it.

          (gen-ast-node
            (progn
              ;; Trees get tagged with a symbol, so AST walks don't get
              ;; confused by tokens.
              (push `(put (car ast-root) 'parser-ast 'production) gen-match-after)

              (if gen-ast-branch
                (push `(parser-ast-add-node ,(parser-gen-attach-transform)) gen-match-effects)
                (setq gen-ast-value `(progn
                                       (parser-ast-add-node (parser-gen-attach-transform))
                                       nil))) ))
          ;; 2. AST transform only.
          ;; within these effects the goal is to change the lexically scoped
          ;; ast-root into either the AST part of the match only or nil.

          (gen-ast-transform
            (if gen-ast-branch
              (push
                `(setq ast-root (funcall ',gen-ast-transform (parser-match-data ast-root)))
                gen-match-effecs)
              (setq gen-ast-value `(funcall ',gen-ast-transform (parser-match-data ast-root)))))

          ;; 3. branch only.

;;;           (gen-ast-branch
;;;             (push `(setq ast-root (parser-match-data ast-root)) gen-match-effects))
          )

        (when gen-ast-branch
          (push `(setq ast-root nil) gen-fail-effects)
          (unless gen-ast-value
            (setq gen-ast-value 'ast-root)))

        (unless gen-ast-value
          (setq gen-ast-value `(parser-match-data ast-root)))
      ))
    ))

(defun parser-gen-logic-phase ( generated )
  "generate the logic phase"

  ;; an interesting experiment for the logic phase would be
  ;; allowing a binary logical operator that combined a logical
  ;; operator with a stateful logical predicate of the ast
  ;; generated. Once the tree walk functions are built this will be
  ;; very powerful.

  ;; the match result by this phase is always logic only as any
  ;; ast has already been consumed.

  (lexical-let
    ((save-result (or
                    gen-match-after

                    (and
                      gen-branch
                      (eq 'match gen-return)))))

    (if save-result
      (progn
        (push `(match ,generated) gen-lexical-scope)
        (setq generated 'match)))

    ;; apply operator
    (if gen-logic-operator
      (setq generated `(,gen-logic-operator ,generated)))

    ;; now we have enough to construct the conditional
    (if gen-branch
      (progn
        ;; first set our rvalue if we will need it.
        (if (eq 'function gen-return)
          (setq gen-match-rvalue t))

        (setq generated
          `(if (parser-match-p ,generated)
             ,@(seq-filter-nil
                 (parser-gen-with-effects gen-match-effects gen-match-rvalue)
                 (parser-gen-with-effects gen-fail-effects  gen-fail-rvalue))))
        ))

    (cond
      ((and gen-branch (or gen-logic-branch (eq 'function gen-return)))
        `(cons ,generated ,gen-ast-value))

      (save-result
        `(cons match ,gen-ast-value))
      (t
        `(cons (car ,generated) ,gen-ast-value))
      )))

(defun parser-gen-lexical-scope ( generated )
  (if gen-lexical-scope
    (progn
      (setq generated
        `(lexical-let*
           ;; the reverse is required so that the lexical bindings
           ;; of the logic phase will be ordered last.
           ,(reverse gen-lexical-scope)

           ,@(car (seq-filter-nil (append
                                    gen-match-after
                                    (list generated)
                                    nil)))))

      (setq gen-match-after nil)))
  generated)

(defun parser-function-generate ( parser-function-semantics )
  (use-dynamic-closure parser-function-semantics

    ;; a sequence requires a predicate to define sequence semantics.
    ;; default to and which makes sense for closures.
    (if (and gen-sequence (not gen-predicate))
      (setq gen-predicate 'parser-predicate-and))

    ;; always turn on trap fail for conditionals.
    (if (and gen-branch (not gen-trap))
      (setq gen-trap t))

    ;; NOTICE: effects cannot be generated until it's known if we
    ;; are branching.

    (parser-gen-ast-effects)
    (parser-gen-input-effects)

    (funcall (if gen-sexp 'seq-filter-nil 'parser-gen-lambda)
      gen-match-before

      ;; can this sequence in name match the description of phases ?
      (list (parser-gen-lexical-scope
              (parser-gen-logic-phase
                (parser-gen-match-phase
                  (parser-gen-match-call)))))

      gen-match-after

      gen-func-rvalue)))

;;----------------------------------------------------------------------
;; parser-function-simplify
;;----------------------------------------------------------------------

;; be extended in the grammar with validation. they can be integrated
;; directly as meta-operators or definitions of custom operators.


;; parser-function-simplify is a key component to the parser compiler.
;; It generates the Match Functions that implement parser semantics.

;; The role in the Parser Compiler design is larger than simply
;; generating code. The API of parser-function-simplify presents
;; parser semantics as composition of parser primitives in parser
;; functions.

;; Parser primitives are the low level functions of a parser such as
;; back-tracking, positive closure, and matching logic.

;; Defining a useful set of orthogonal primitives, conditional
;; branching, and user defined hooks allows parsing to be extended in
;; the grammar while keeping the parser code generated instead of hand
;; written.

;; be extended in the grammar with validation. they can be integrated
;; directly as meta-operators or definitions of custom operators.

;; Bloat:

;; what used to be defined in a single place is now generated directly into
;; the parser. The impact of this needs to be measured.

;; It should be possible to determine if two parser functions are equivalent.
;; If so I can bind that function and use a symbol instead of generating a
;; full body each time. I could also possibly parameterize it, so that I
;; could bind a common part, and bind parameters in an anonymous lambda.

(defun parser-set-once ( var value )
  "Set a value to the semantics table, throw semantic-error if it
   the symbol has already been set"
  (if (symbol-value var)
    (throw 'semantic-error 'set-once)

    (progn
      (unless (or (functionp value) value)
        (throw 'semantic-error 'nil-data))
      (set var value))))

(defun parser-set-nil ( var value )
  "Set a value in the semantics table if it is currently nil.
   throw semantic-error only if the new value is nil."
  (unless (symbol-value var)
    (unless (or (functionp value) value)
      (throw 'semantic-error 'nil-data))
    (set var value)))

(defun parser-function-validate ( semantics )
  "Validate the high level specification, generate default code fragments."

  (save-lexical-closure semantics
    (if (or gen-ast-transform gen-closure gen-sequence)
      (parser-set-nil 'eff-ast t))

    (if (or gen-ast-branch gen-input-branch gen-logic-branch)
      (parser-set-nil 'gen-branch t))

    (if gen-branch
      (parser-set-nil 'gen-trap t))
    ) t)

(defun parser-function-reduce ( semantics &rest statements )
  ;; a proper reduce would catch the semantic errors, generate the function
  ;; from the original, and then return a new table with the modifications
  ;; made.

  (lexical-let
    ((merged-semantics (copy-scope semantics)))

    (save-lexical-closure merged-semantics
      (mapc
        (lambda (s)
          (catch 'next
            (unless s (throw 'next nil))

            (lexical-let
              ((primitive (if (consp s) (car s) s))
               (data      (if (consp s) (eval (cadr s)))))

              (cond
                ;; high level functions.

                ;; the match call
                ((eq primitive 'predicate)  (parser-set-once 'gen-predicate data))

                ((eq primitive 'sequence)   (parser-set-once 'gen-sequence data))
                ((eq primitive 'greedy)     (parser-set-once 'gen-closure 'parser-predicate-greedy))

                ;; input effects
                ((eq primitive 'input-branch)
                  (progn
                    (parser-set-once 'eff-input t)
                    (parser-set-once 'gen-input-branch t)))

                ((eq primitive 'input-discard) (parser-set-once 'eff-input t))

                ;; ast effects

                ;; discard is mutually exclusive with the other ops, which are
                ((eq primitive 'ast-discard)
                  (progn
                    (parser-set-once 'eff-ast t)
                    (parser-set-once 'gen-ast-discard t)))

                ((eq primitive 'ast-branch)
                  (progn
                    (when gen-ast-discard (throw 'semantic-error 'invalid))

                    (parser-set-nil 'eff-ast t)
                    (parser-set-once 'gen-ast-branch t)))

                ((eq primitive 'ast-node)
                   (progn
                    (when gen-ast-discard (throw 'semantic-error 'invalid))

                     (parser-set-nil 'eff-ast t)
                     (parser-set-once 'gen-ast-node data)))

                ((eq primitive 'ast-transform)
                  (progn
                    (when gen-ast-discard (throw 'semantic-error 'invalid))

                    (parser-set-nil 'eff-ast t)
                    (parser-set-once 'gen-ast-transform data)))

                ;; low-level interface.
                ((eq primitive 'closure)    (parser-set-once 'gen-closure data))
                ((eq primitive 'trap-fail)  (parser-set-nil  'gen-trap t))
                ))))  statements)

      )
    merged-semantics))

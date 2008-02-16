;;----------------------------------------------------------------------
;; parser.el
;; Primary Author: Mike Mattie (codermattie@gmail.com)
;; Copyright (C) 2008 Mike Mattie
;; Description: Recursive Descent parser compiler with a lisp macro
;;              interface.
;;----------------------------------------------------------------------

;; parser.el is a experimental parser-compiler DSL.

;; ->Hypothesis

;; Exposing the internal semantics of the generated parser as a well
;; defined programming interface allows a parser writer to freely mix
;; the semantics of a grammar classes such as CFG and PEG with
;; tailored parsing behavior. The programming interface is
;; sufficiently powerful that user defined functions can be
;; constrained to a few typed hooks without losing the ability to
;; define useful or unusual parsers.

;; ->Benefits

;; A well defined programming interface has the following significant
;; advantages.

;; Increasing the generality of the parser compiler beyond a single
;; grammar class brings more of the parser within formal validation
;; reducing the opportunity to introduce defects.

;; The parser definition is more concise since the general hosting
;; language is less often needed to complete a parser.

;; This compiler facilitates new parsing operators, making it a good
;; vehicle for experimenting with new grammar classes.

;; ->Features

;; 0. Parser compilation is integrated directly into lisp
;;    with a macro DSL that returns an entry point.

;; 1. The DSL is sugared with grammar classes and a define
;;    syntax keeping the parser definitions concise for
;;    both simple and complex parsers.

;; This experiment calls forth the four horsemen of the Lisp
;; Apocalypse: eval,apply,lambda,macro.

;; ->TODO

;;   -> Phase 1: correctness

;; 0. new parser generator merged in, and debugged.

;; 2. replace the generated cond tables in the grammar interpreter with hash tables

;; 1. All of the PEG predicates (missing not)

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

;;    -> Phase 3: Experiments

;; Longest

;; make a longest possible match operator. this would involve a primitive
;; that would scope a token match counter. The recursive match with the
;; highest token count would be returned. This approach to longest match
;; assumes packrat optimization, and that constructing ast is a cheap
;; operation.

;; Lazy

;; is it possible to make the parser lazy instead of greedy with a
;; closure while still being top-down ?  with packrat optimization an
;; interesting algorithm for efficiently constructing a correct lazy
;; match for a sequence might be possible.

;; If such an algorithm was found enabling something like CFG
;; semantics then the parser compiler would be a much more interesting
;; beast.

;; lazy may need to be a sequence relational operator but that is quite odd.
;; can top-down work right to left ? maybe I could scan back from a sync token
;; and do the sequence matching in reverse.

;; A B C

;; A runs match. If it works it lets B take a crack at the next run. If B doesn't
;; match it goes back to A for more greed. If B does match then B becomes A and
;; C is bound as B until the sequence is exhausted.

;; Since the tokens never actually implement greed themselves this may
;; actually work. implications of recursively buried greed ?

;; 2. tracing at the primitive level, generating instrumented parser functions.

;; 3. Could tokens be emacs functions that bound the existing syntax anaylsis
;;    of a buffer ? kind of a high level data structure builder on-top of the
;;    existing function ?

;; Badly formed inputs: a way to terminate a previous match ? something like the
;; proposed super brace ? would it help with stuff like HTML parsing ?

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

;; TODO: can this be a defun ? if it can it should.
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
      ((toggle (eval (cons 'or (mapcar (lambda ( trace-on )
                                    ;; eq comparison of symbols does not work. A string
                                    ;; comparison is used for matching.

                                    ;; FIXME: symbol-value may remove this wart.
                                    (if (equal (symbol-name production) (car trace-on))
                                      (cdr trace-on)))
                            parser-trace) ))))
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

;; The NEW Result Match interface.

;; TODO: migrate the token generator to parser-result-token.

(defun parser-result-logic ( result )
  "The logical part of a Match Result cons cell is returned."
  (car result))

(defun parser-result-ast ( result )
  "The AST part of a Match Result cons cell is returned."
  (cdr result))

(defun parser-match-p ( result )
  "return true if the logic is a match result"
  (and (consp result) (parser-result-logic result)))

(defun parser-result-negate ( result )
  "negate the Match Result RESULT toggling the logical sense of the Match Result while
   preserving the AST value."
  (cons
    (if (parser-result-logic result)
      nil
      t)
    (parser-result-ast result)))

(defun parser-result-match ( &optional ignore )
  "return a data-less positive Match Result. Can be used as the
   logical optional operator with an optional argument that is
   ignored."
  (cons t nil))

(defun parser-result-token ( ast )
  "Return a token Match Result."
  (cons (parser-consumed) ast))

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
      ((match-status (parser-result-logic match-result))
       (match-data   (parser-result-ast match-result)))

      (if (numberp match-status)
        (progn
          (parser-advance match-status)

          (unless (and (symbolp match-data) (null match-data))
            (parser-ast-add-node match-data))

          (throw 'consumed-match (parser-result-match)))

        ;; The alternative to a token is a possible un-consumed
        ;; production. Consumption occurs regardless of the logical
        ;; sense of the Match Result. This keeps logic and AST
        ;; orthogonal.

        ;; It is a trivial change to make consumption conditional if
        ;; the orthogonality is found to be too strange, but I doubt
        ;; this orthogonality will be visible at the semantics level
        ;; unless the user is deliberately introducing it.

        (when match-data
          (if (parser-ast-p match-data)
            (parser-ast-merge-node match-data)
            (parser-ast-add-node match-data))

          (throw 'consumed-match
            (if match-status
              (parser-result-match)
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

;; and/or are sequence relational operators.

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
  (dolist (match-func match-list (parser-result-match))
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
    (if matched-once (parser-result-match)) ))

;;----------------------------------------------------------------------
;; parser-function-generate
;;----------------------------------------------------------------------

;; The Parser Function Generator generates Match Functions from a set
;; of parser primitives.

;; -> Semantics Table

;; The parser-function-semantics table is the data structure that
;; glues these generator functions together. It is a set of flags and
;; code fragments.

;; The compile part ensures that the semantics table is complete
;; including dependencies such as normally invisible generation
;; flags implied by the combination of semantics.

;; The generate part fills the semantics table with the necessary code
;; fragments based on the flags and interpolates the fragments into a
;; lambda or sexp.

(setq parser-function-semantics
  (make-closure

    (gen-sexp           nil) ;; generate sexp form when t, more generalized entry
                             ;; point may be the right thing.

    ;; Match Phase
    (gen-sequence       nil)  ;; a set of Match functions that imply gen-predicate
                              ;; as a relational operator.

    (gen-predicate      nil)  ;; a single Match Function or a relational operator.
    (gen-closure        nil)  ;; pass the match call to closure for delayed evaluation.

    ;; Eval Phase

    ;; Parser Effects
    (eff-input          nil)
    (gen-input-branch   nil)  ;; do input effects branch ?

    ;; logical effects
    (eff-logic          nil)

    (gen-function-operator nil)
    (gen-eval-operator nil)

    ;; ast effects
    (eff-ast            nil)

    (gen-ast-discard    nil)  ;; discard is mutually exclusive with the set below.
                              ;; discard the ast tree.

    (gen-ast-transform  nil)  ;; user defined AST transform.
    (gen-ast-node       nil)  ;; Create named AST trees that are attached instead
                              ;; of merged.
    (gen-ast-branch     nil)  ;; do ast effects branch ?

    ;; Function Phase

    (gen-function      'eval) ;; match or eval, return the eval logical result, or the match
                              ;; logical result.

    ;; Validation and Generation Internals

    (gen-trap           nil)  ;; internal, always turned on when needed. trap
                              ;; parser-match-fail

    (gen-branch         nil)  ;; do we have branching ?

    ;; code fragments

    (gen-ast-value      nil)  ;; the result of a AST effect that is not a node.
                              ;; will be combined with the logical result to
                              ;; keep ast/logic orthogonal.

    ;; list of bindings required by evaluation. These are largely due
    ;; to side? effect ordering.
    (gen-lexical-scope  nil)

    ;; list of bindings for a recursion environment, currently used to
    ;; scope the AST tail.
    (gen-dynamic-scope  nil)

    ;; Functions that modify the parser/AST state need to run a setup before
    ;; the evaluation phase, and place their finish effects in either the
    ;; gen-eval-always fragments, or the branch fragments.

    (gen-eval-setup     nil)
    (gen-eval-always    nil)

    ;; branching fragments.

    (gen-match-effects  nil)
    (gen-match-rvalue   nil)

    (gen-fail-effects   nil)
    (gen-fail-rvalue    nil)))

(defun parser-prune-lambda ( &rest generated )
  "parser-prune-lambda inserts a sequence of fragments into a lambda form pruning
   nil fragments."
  `(lambda ()
     ,@(apply 'append (list-filter-nil generated))))

(defun parser-gen-match-call ()
  "The match call of a parser function includes the closure, predicate,
   and sequence. Evaluation produces the logical Match Result of the
   function after parser-consume-match strips any AST.

   inputs: gen-predicate, gen-sequence, gen-closure"
;; FIXME: only quote symbols, lists should be unquoted. PITA = defun.
  `(parser-consume-match
     ,(lexical-let
       ((predicate nil))

       (if gen-sequence
         (progn
           (setq predicate `(apply ',gen-predicate '(,@gen-sequence)) )
           (if gen-closure
             `(funcall ',gen-closure ,(parser-prune-lambda predicate))
             predicate))

         (if gen-closure
           `(funcall ',gen-closure ',gen-predicate)
           `(funcall ',gen-predicate) )))))

(defun parser-prune-recursion-environment ( generated )
  "parser-prune-recursion-environment interpolates the sexp generated
   into a dynamic scope with the bindings of gen-dynamic-scope
   from the semantics table.

   inputs: gen-dynamic-scope"
  (if gen-dynamic-scope
    `(let
       ,gen-dynamic-scope
       ,generated)
  generated))

(defun parser-prune-match-phase ( generated )
  "The match phase traps any parser-match-fail non-local exists
   from the recursion of the match call. This trap is only
   generated when there is conditional evaluation.

   inputs: gen-trap"
  (lexical-let
    ((transform (parser-prune-recursion-environment generated)))

    (if gen-trap
      `(catch 'parser-match-fail
         ,transform)
      transform)))

(defun parser-gen-input-effects ()
  "Create the input effects of the Parser Function.

   input: eff-input, gen-branch, gen-input-branch
   modifies: gen-eval-setup, gen-match-effects, gen-eval-always"
  (catch 'done
    (unless eff-input (throw 'done nil))

    (push `(parser-push) gen-eval-setup)

    (if (and gen-branch gen-input-branch)
      (progn
        (push `(parser-pop) gen-match-effects)
        (push `(parser-backtrack) gen-fail-effects))
      (push `(parser-pop) gen-eval-always)) ))

;; AST effects

(defun parser-gen-node-transform ()
  "If gen-ast-transform is set Create the call to a AST transform
   function. The identity of the AST before the transform is
   preserved and returned with the transform result. Otherwise
   return the AST value.

   input: gen-ast-transform"
  (if gen-ast-transform
    `(cons (car ast-root) (cdr (funcall ',gen-ast-transform ast-root)))
    'ast-root))

(defun parser-gen-ast-effects ()
  "Generate any AST effects for the parser function.

   The most basic AST effect is creating a AST node or list which
   consists of a lexically scoped head and a dynamically scoped
   tail.

   The first element of the list is always an identity symbol
   with the parser-ast property production. AST nodes that are
   not explicitly named are identified with a null symbol.

   The flag eff-ast alone will generate a new AST node that
   shadows the existing AST tree.

   The remaining set of AST effects is either a branch-able
   gen-ast-discard or one of the effects below.

   gen-ast-node: An identity symbol is given to name the
   node. It is marked with the parser-ast property production.
   A transform can be combined with a node.

   A node is immediately attached with parser-ast-add-node so
   that the node is added rather than merged into the parent AST
   tree.

   gen-ast-transform: The given function receives either a
   token or a AST node. The transform must produce a cons cell,
   either a AST tree rooted with a AST node or arbitrary token
   data.

   gen-ast-value is either nil or a fragment that is the AST
   evaluation value.

   inputs: eff-ast, gen-ast-discard, gen-ast-node, gen-branch,
           gen-ast-branch, gen-ast-transform

   outputs: gen-ast-value, gen-lexical-scope, gen-dynamic-scope,
            gen-eval-always,gen-match-effects
   "

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
        (push `(setq ast-root nil) gen-eval-always)
        (setq gen-ast-value 'nil))

      (progn
        ;; a conditional selects the highest combinational complexity
        ;; as the entry point for generating the code.

        (cond
          (gen-ast-node
            (progn
              (push `(put (car ast-root) 'parser-ast 'production) gen-eval-always)

              (if gen-ast-branch
                (progn
                  (push `(parser-ast-add-node ,(parser-gen-node-transform)) gen-match-effects)
                  (setq gen-ast-value nil))
                (setq gen-ast-value `(progn
                                       (parser-ast-add-node ,(parser-gen-node-transform))
                                       nil))) ))
          (gen-ast-transform
            (if gen-ast-branch
              (push
                `(setq ast-root (funcall ',gen-ast-transform ast-root))
                gen-match-effects)
              (setq gen-ast-value `(funcall ',gen-ast-transform ast-root)))) )

        (when (and
                gen-ast-branch
                (not gen-ast-node))

          ;; the code generated assumes that side effects during a
          ;; left to right evaluation will be visible in a rightmost
          ;; value modified in a sexp evaluated left.

          (push `(setq ast-root nil) gen-fail-effects)
          (unless gen-ast-value
            (setq gen-ast-value 'ast-root)))
        ))
    ))

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

(defun parser-eval-disjunct-p ()
  (or
    ;; ordering of eval-always forces a temporary
    ;; save of the Match Result
    gen-eval-always

    ;; Save the Match Result from the match call
    ;; because we will evaluate it twice.
    (and
      gen-branch
      (eq 'match gen-function))))

(defun parser-prune-eval-phase ( generated )
  "Generate the evaluation phase stub."
  ;; an interesting experiment for the eval phase would be
  ;; allowing a binary logical operator that combined a logical
  ;; operator with a stateful logical predicate of the ast
  ;; generated. Once the tree walk functions are built this will be
  ;; very powerful.

  ;; the match result by this phase is always logic only as any
  ;; ast has already been consumed.

  (lexical-let
    ((eval-disjunct (parser-eval-disjunct-p)))

    (when eval-disjunct
      (push `(match ,generated) gen-lexical-scope)
      (setq generated 'match)

      (when gen-branch
        ;; if the evaluation result is the function logical result set
        ;; the match rvalue.
        (setq gen-match-rvalue t)))

    ;; apply an evaluation logical operator, but only after the match
    ;; result has been saved if necessary.
    (if gen-eval-operator
      (setq generated `(,gen-eval-operator ,generated)))

    ;; now we have a fragment we can conditionally evaluate for the branch
    ;; in generated.
    (if gen-branch
      `(if (parser-match-p ,generated)
         ,@(seq-filter-nil
             (parser-gen-with-effects gen-match-effects gen-match-rvalue)
             (parser-gen-with-effects gen-fail-effects  gen-fail-rvalue)))
      generated) ))

(defun parser-prune-result-operator ( generated )
  (if gen-function-operator
    `(,gen-function-operator generated)
    generated))

(defun parser-eval-conjunct-p ()
  (eq 'eval gen-function))

(defun parser-prune-result-phase ( generated )
  (lexical-let
    ((eval-phase (parser-prune-result-operator
                   (if gen-lexical-scope
                     `(lexical-let*
                        ;; the reverse is required so that the lexical bindings
                        ;; of the logic phase will be ordered last.
                        ,(reverse gen-lexical-scope)

                        ,@(if gen-eval-always
                            (append gen-eval-always generated)
                            generated))
                     generated))))

    (if (or gen-branch gen-ast-value)
      `(cons
         (if (and gen-branch (eval-conjunct-p))
           eval-phase
           `(parser-match-p ,eval-phase))
         ,gen-ast-value)
      eval-phase)))

(defun parser-function-generate ( parser-function-semantics )
"
The goal of this generator design is to produce elisp that
appears more human, where human is opportunistic pruning of
redundant code.

To do this a set of valid primitives in the form of a closure is
taken as an input. It is assumed that this set is the largest
possible set of primitives taken from a sequence of primitives.

The largest valid set is computed largely by parser-semantic-union.

The minimal structure is a ordered sequence of a match phase
evaluation phase, and function phase.

The match phase structure has two major elements: a match
function call and a optional dynamically scoped environment for
the recursion.

The evaluation phase is can manipulate the parser and the AST
state with optional branching on the match result.

The function phase composes a match result with the logic
and ast parts from either the match phase or evaluation phase.
"
  (use-dynamic-closure parser-function-semantics

    (unless gen-predicate
      (throw 'semantic-error 'invalid))

    ;; any time we have a closure, or sequence, we will always need to
    ;; setup a basic AST effect, even if AST effects were not explicitly
    ;; set by AST effects.

    (when (or gen-closure gen-sequence)
      (setq eff-ast t))

    ;; effects cannot be generated until it's known if we are
    ;; branching.
    (when (or gen-ast-branch gen-input-branch gen-logic-branch)
      (setq gen-branch t))

    ;; whenever there is conditionality trap match-fail.
    (when gen-branch
      (setq gen-trap t))

    (parser-gen-ast-effects)
    (parser-gen-input-effects)

    (funcall (if gen-sexp 'seq-filter-nil 'parser-prune-lambda)
      gen-eval-setup

      (list
        (parser-prune-result-phase
          (parser-prune-eval-phase
            (parser-prune-match-phase
              (parser-gen-match-call))))) )))

;;----------------------------------------------------------------------
;; parser-semantic-union
;;----------------------------------------------------------------------


(defun parser-fold-primitive ( var value )
  "Discard subsequent attempts to merge the semantic VAR assuming
   that VALUE is a constant of all parser functions. Throw a
   semantic-error 'incomplete only when the function and value of
   VALUE are nil or void."
  (unless (symbol-value var)
    (unless (or (functionp value) value)
      (throw 'semantic-error 'incomplete))
    (set var value)))

(defun parser-merge-primitive ( var value )
  "Set a value to the semantics table, assuming that valuable
   is a variable value between parser functions.

   Throw semantic-error 'collision if it the symbol has already
   been set."
  (if (symbol-value var)
    (throw 'semantic-error 'collision)

    (progn
      (unless (or (functionp value) value)
        (throw 'semantic-error 'incomplete))
      (set var value))))

(defun parser-merge-exclusive ( mutex )
  "When semantics are exclusive within a set of effects or a
   phase a flag is used so that the first merge succeeds but
   subsequent merges fail.

   MUTEX variable is the sole argument. semantic-error 'collision
   is thrown for subsequent attempts to set the mutex."
  (if (symbol-value mutex)
    (throw 'semantic-error 'collision)
    (set mutex t)))

(defun parser-exclusive-primitive ( mutex var value )
  (parser-merge-exclusive mutex)
  (parser-merge-primitive var value))

(defun parser-combine-primitive ( mutex var value )
  "This merge combines within a effect or phase mutex however
   the semantic variable is still exclusive."
  (parser-fold-primitive mutex t)
  (parser-merge-primitive var value))

(defun parser-combine-exclusive-of ( mutex of-mutex var value )
  "stub."
  (when (symbol-value of-mutex)
    (throw 'semantic-error 'collision))

  (parser-fold-primitive mutex t)
  (parser-merge-primitive var value))

(defun parser-semantic-union ( semantics tape )
  "parser-semantic-union

   Merges semantic primitives from list TAPE into SEMANTICS.
   Each element of TAPE is a symbol or list pair evaluated
   as a Parser Semantic Primitive. SEMANTICS is a valid set of
   primitives as a closure.

   A greedy iteration attempts to produce a valid union
   of SEMANTICS and the primitives of TAPE. A valid union
   updates SEMANTICS and consumes the primitive from TAPE.

   An invalid union or exhausting TAPE halts parser-semantic-union.
   A diagnostic of the halt a symbol of: finished invalid unknown
   is cons'd with the unconsumed tape if any."

  (lexical-let*
    ((i-current (car tape))
     (tape-next (cdr tape))

     (primitive (if (consp i-current) (car i-current) i-current))
     (data      (if (consp i-current) (eval (cadr i-current))))

     (merge
       (catch 'semantic-error
         (save-lexical-closure semantics
           (cond
             ;;----------------------------------------------------------------------
             ;; call phase
             ;;----------------------------------------------------------------------

             ((eq primitive 'greedy)      (parser-merge-primitive 'gen-closure   'parser-predicate-greedy))

             ((eq primitive 'relation-or  (parser-merge-primitive 'gen-predicate 'parser-predicate-or)))
             ((eq primitive 'relation-and (parser-merge-primitive 'gen-predicate 'parser-predicate-and)))

             ;;----------------------------------------------------------------------
             ;; effects phase
             ;;----------------------------------------------------------------------

             ;; -> logic effects

             ((eq primitive 'return-match)
               (parser-combine-primitive-exclusive-of 'gen-function-operator
                 'eff-logic
                 'gen-function 'match))

             ((eq primitive 'always-match)
               (parser-combine-primitive
                 'eff-logic
                 'gen-function-operator 'parser-result-match))

             ((eq primitive 'negate-function)
               (parser-combine-primitive
                 'eff-logic
                 'gen-function-operator 'parser-result-negate))

             ((eq primitive 'negate-eval)
               (parser-combine-primitive
                 'eff-logic
                 'gen-eval-operator 'parser-result-negate))

             ;; -> input effects

             ((eq primitive 'input-branch)
               (parser-exclusive-primitive
                 'eff-input
                 'gen-input-branch t))

             ((eq primitive 'input-discard)
               (parser-merge-exclusive 'eff-input))

             ;; -> ast effects

             ;; discard is mutually exclusive with the other ops, which are
             ((eq primitive 'ast-discard)
               (parser-exclusive-primitive
                 'eff-ast
                 'gen-ast-discard t))

             ((eq primitive 'ast-branch)
               (parser-combine-exclusive-of 'gen-ast-discard
                 'eff-ast
                 'gen-ast-branch t))

             ((eq primitive 'ast-node)
               (parser-combine-exclusive-of 'gen-ast-discard
                 'eff-ast
                 'gen-ast-node data))

             ((eq primitive 'ast-transform)
               (parser-combine-exclusive-of 'gen-ast-discard
                 'eff-ast
                 'gen-ast-transform data))

             ;; when we don't know what it is.
             ((throw 'semantic-error 'unkown)))

           ;; All of the instructions above will terminate the match
           ;; call phase. Ensure that gen-predicate is set correctly
           ;; so subsequent match calls will fail to merge.

           (when (not gen-predicate)
             (if (> (length gen-sequence) 1)
               ;; the default relational operator is and. to get a
               ;; specific relational operator make sure it's the
               ;; first instruction after the call phase.
               (setq gen-predicate 'parser-predicate-and)

               ;; simple optimization, when a sequence has only a single call
               ;; make it the predicate.
               (progn
                 (setq gen-predicate (car gen-sequence))
                 (setq gen-sequence nil))))

          nil))))

    (cond
      ((eq merge 'incomplete)
        (signal 'parser-semantic-error
          (format "incomplete instruction %s is a fatal error." (pp-to-string i-current))))

      ((null merge)           (if tape-next
                                (parser-function-reduce semantics tape-next)
                                (cons 'finished nil)))
      ((eq merge 'collision)  (cons 'invalid tape))
      ((eq merge 'unkown)     (cons 'unkown  tape))
      )))

;;----------------------------------------------------------------------
;; parser-semantic-interpreter - Welcome to the Machine.
;;----------------------------------------------------------------------

(defvar parser-mtable-init-size 13
  "initial size of the match-table objarray for storing match functions. the value
   was chosen based on the recommendation of prime numbers for good hashing.

   this value is a WAG. I still do not know how to compute a good value.")

(defun parser-make-symbol-table ()
  "create a Match Function symbol table which is an objarray"
  (make-vector parser-mtable-init-size 0))

(defun parser-link-function ( identifier &optional definition )
  "Retrieve or define a Match Function. the name of the production is required,
   as well."
  (lexical-let
    ((identity (symbol-name identifier)))

    (if (and
          (functionp (intern identity match-table))
          definition)

      (signal 'parser-semantic-error (format "illegal redefinition of Match Function %s" identity))

      (when definition
        (fset (intern identity match-table) (eval definition))))

    (intern identity match-table)))

(defun parser-call-function ( semantics match-function )
  "match-function is pushed onto the current sequence, returns t
   if there was a semantic collision, nil otherwise."
  (save-lexical-closure semantics
    (if gen-predicate
      t
      (progn
        (push match-function gen-sequence)
        nil)) ))

(defun parser-semantic-interpreter-run ( machine-state )
  "parser-semantic-interpreter-run takes a machine-state
   argument and resumes compiling parser functions by executing
   instructions.

   This function's portion of the instruction set recognized
   is concerned with compilation whereas parser-function-reduce
   focuses on merging primitives into a set of semantics for
   a Parser Function."

  (lexical-let
    ((unrecognized nil)
     (compiled  nil)
     (semantics (car machine-state))
     (tape      (cdr machine-state)))

    (while tape
      (lexical-let*
        ((i-current (car tape))
         (tape-next (cdr tape))

         (instruction (if (consp i-current) (car i-current) i-current))
         (data        (if (consp i-current) (eval (cadr i-current)))))

        (setq tape
          (cond
            ;; strict order link gets first crack at the volatile compiled
            ;; register before it disappears.

            ((eq instruction 'link)
              (progn
                (unless compiled
                  (signal 'parser-semantic-error "link instruction failed with a nil compiled register"))

                (setq unrecognized nil)

                (parser-link-function data compiled)
                (setq compiled nil)

                tape-next))

            ;; flush compiled if a link instruction has not consumed it.
            (compiled
              (progn
                (when (parser-call-function semantics compiled)
                  (signal 'parser-semantic-error
                    "impossible !? attempt to flush volatile compiled register resulted in a semantic collision"))

                (setq unrecognized nil)

                (setq compiled nil)

                tape))

            ;; order is now arbitrary.

            ((eq instruction 'compile)
              (progn
                (setq unrecognized nil)

                (setq compiled (parser-function-generate semantics))
                (setq semantics (copy-closure parser-function-semantics))

                tape-next))

            ((eq instruction 'call)
              (progn
                (setq unrecognized nil)

                (if (parser-call-function semantics (parser-link-function data))
                  (cons 'compile tape)
                  tape-next)))

            ((if unrecognized
               (signal 'parser-semantic-error (format
                                                "unrecognized instruction %s" (pp-to-string i-current)))
               (lexical-let*
                 ((semantic-halt (parser-semantic-union semantics tape))
                   (diagnostic (car semantic-halt)))

                 (cond
                   ((eq diagnostic 'finished) nil)
                   ((eq diagnostic 'invalid) (cons 'compile (cdr semantic-halt)))
                   ((eq diagnostic 'unkown)
                     (progn
                       (setq unrecognized t)
                       (cdr semantic-halt))) )) ))
            ))))

    (when compiled
      (signal 'parser-semantic-error
        "error: exiting parser-semantic-interpreter-continue with non-nil compiled register."))

    (cons semantics tape) ))

(defun parser-semantic-interpreter-start ( machine-state instructions )
  (parser-semantic-interpreter-continue
    (cons (if machine-state
            machine-state
            (copy-closure parser-function-semantics))
      instructions)))

(defun parser-semantic-interpreter-continue ( machine-state instructions )
  "stub for when sugaring is put in")

(defun parser-semantic-interpreter-terminate ( machine-state )
  (lexical-let
    ((entry-point (catch 'semantic-error
                    (parser-function-generate (car machine-state)))))

    (when (eq 'invalid entry-point)
      (signal 'parser-semantic-error (format "terminated with error: %s" (symbol-name entry-point))))

    entry-point))


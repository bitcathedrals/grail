;;----------------------------------------------------------------------
;; diagnostics
;;----------------------------------------------------------------------

;; test the diagnostics of the parser. I think some sort of eval may be necessary
;; in diagnostics.

;; see what the expansion looks like
(pp (macroexpand
      (parser-diagnostic `(foo bar)
        "test"
        "the right thing")))

;; see what it looks like when handled by message.
(message "%s"
      (parser-diagnostic `(foo bar)
        "test"
        "the right thing"))))

;;----------------------------------------------------------------------
;; parser function generation testing.
;;----------------------------------------------------------------------

(parser-call-function (copy-closure parser-function-semantics) 'foo)

;; make it read-only after the wipe and insert, make it elisp with highlighting.
;; then it might be worthy as a utility function.
(defun parser-semantic-dump ( &rest instructions )
  "dump the code generated by the semantic compiler"
  (lexical-let
    ((compiled
       (let
         ((match-table (parser-make-symbol-table)))

         (parser-semantic-interpreter-terminate
           (parser-semantic-interpreter-start nil instructions))) ))

    (if (listp compiled)
      (lexical-let
        ((review-buffer (get-buffer-create "generated")))

        (with-current-buffer review-buffer
          (erase-buffer)
          (insert (pp-to-string compiled)))
        (pop-to-buffer review-buffer t))
      (message "failed!: %s" (symbol-name compiled))) ))

;; test the single token/function case

;; gen-logic-branch is a void variable. kill the stale code.
(parser-semantic-dump `(call 'token))

;; check an explicit sequence
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  `(call 'baz)
  'relation-or)

;; check the default sequence
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  `(call 'baz))

;; test the closure case
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'greedy)

;; test input effects
(parser-semantic-dump
  `(call 'foo)
  'input-discard)

;; test input effects and branching
(setq test-function (parser-function-reduce parser-function-semantics
            'input-branch
            `(sequence '(foo bar baz))))

;; test the greedy closure with input branch.
(setq test-function (parser-function-reduce parser-function-semantics
            'greedy
            'input-branch
            `(sequence '(foo bar baz))))

;; test AST discard
(setq test-function (parser-function-reduce parser-function-semantics
            'ast-discard
            `(sequence '(foo bar baz))))

(setq test-function (parser-function-reduce parser-function-semantics
            'ast-discard
            'input-branch
            `(sequence '(foo bar baz))))

;; test the AST conditional
(setq test-function (parser-function-reduce parser-function-semantics
            'ast-branch
            `(sequence '(foo bar baz))))

;; test the AST transform.
(setq test-function (parser-function-reduce parser-function-semantics
            `(ast-transform 'transform-foo)
            `(sequence '(foo bar baz))))

(setq test-function (parser-function-reduce parser-function-semantics
            `(ast-node 'left-prod)
            `(ast-transform 'transform-foo)
            `(sequence '(foo bar baz))))

(setq test-function (parser-function-reduce parser-function-semantics
            'ast-branch
            `(ast-transform 'transform-foo)
            `(sequence '(foo bar baz))))

;; AST node.
(setq test-function (parser-function-reduce parser-function-semantics
            `(ast-node 'prod-foo)
            `(sequence '(foo bar baz))))

;; something that looks like a left production.

(setq test-function (parser-function-reduce parser-function-semantics
             'ast-branch
             'input-branch

            `(ast-node 'prod-foo)
            `(sequence '(foo bar baz))))

(setq test-function (parser-function-reduce parser-function-semantics
             'ast-branch
             'input-branch

            `(ast-node 'prod-foo)
            `(ast-transform 'transform-foo)
            `(sequence '(foo bar baz))))

(parser-semantic-interpreter-start nil
  'ast-branch

  `(ast-node 'prod-foo)
  `(ast-transform 'transform-foo)
  `(sequence '(foo bar baz))))

(pp-closure test-function)

;;----------------------------------------------------------------------
;; parser-function-simplify testing.
;;----------------------------------------------------------------------

;; any time there are more than one match function we need a primitive
(pp (parser-function-simplify 'parser-primitive-and '(foo bar baz)))

(pp (parser-function-simplify 'singleton nil
      ))

;; see what happens when closures are thrown into the mix.
(pp (parser-function-simplify 'parser-primitive-and '(foo bar baz)
      `(gen-closure 'parser-primitive-greedy)
      ))

(pp (parser-function-simplify 'singleton nil
      `(gen-closure 'parser-primitive-greedy)
      ))

;; try out trap fail.
(pp (parser-function-simplify 'singleton nil
      'trap-fail
      `(gen-closure 'parser-primitive-greedy)
      ))

;; try out the lexical stuffs
(pp (parser-function-simplify 'singleton nil
      'trap-fail
      'matchahead
      `(gen-closure 'parser-primitive-greedy)
      ))

;; try out the cond stuff
(pp (parser-function-simplify 'singleton nil
      'trap-fail
      'backtrack
      `(gen-closure 'parser-primitive-greedy)
      ))

(pp (parser-function-simplify 'parser-primitive-and '(foo bar baz)
      'trap-fail
      'backtrack
      `(gen-closure 'parser-primitive-greedy)))

;; this is what an and sequence should look like
(pp (parser-function-simplify 'parser-primitive-and '(foo bar baz)
      `(gen-match-effects `(parser-?consume-match ast-root))
      `(gen-match-rvalue  `(parser-make-production-match nil))

      'trap-fail
      'backtrack
      'ast-newroot))

;; this is what a left production should look like, note that
;; append-element is used instead of ?consume-match, so that
;; the tree is attached as a node instead of a list.
(pp  (parser-function-simplify 'parser-predicate-and match-list
    `(gen-ast-root   'prod-foo)


    `(gen-match-rvalue  `(parser-make-production-match nil))

    'backtrack
    'ast-new-production
    'ast-attach-node
    'trap-fail))

(defun parser-ast-descend ( non-terminal func-match )
  "start a new AST level"
  (catch 'parser-match-fail
    (lexical-let
      ((unattached-node (cons non-terminal nil)))

      ;; this step populates the detached node
      (let
        ((parse-tree unattached-node))
        (parser-?consume-match (funcall func-match)) )

      ;; Attach the tree or return it as the completed AST.
      (if (boundp 'parse-tree)
        (progn
          (parser-ast-append-element unattached-node)
          (parser-make-production-match nil))
        (parser-make-production-match unattached-node)) )))




            `(lambda ( start-pos )
               (let
                 ((parser-position (cons start-pos nil))) ;; initialize the backtrack stack
                 (save-excursion
                   (goto-char start-pos)
                   ;; note that the start symbol of the grammar is built in as an or combination
                   ;; of the top-level definitions.
                   (lexical-let
                     ((parse (,(parser-compile-production 'start
                                 (parser-primitive-function 'parser-or definition))) ))
                     (if parse
                       ;; if we have a production return the position at which the
                       ;; parser stopped along with the AST.
                       (cons (parser-pos) (parser-match-data parse))
                       nil))
                   )))

;;----------------------------------------------------------------------
;; token interp phase
;;----------------------------------------------------------------------

(defmacro test-interp-token ( &rest syntax )
  (lexical-let
    ((compile (catch 'syntax-error
                (parser-interp-token (cdr syntax)))))

    (if (stringp compile)
      (message "%s" compile)
      compile)
    ))

(defun run-test-interp-token ( match-function )
  (lexical-let
    ((result (funcall match-function)))
    (message "TI match? %s AST %s" (if (car result) "yes" "no") (pp (cdr result)))
    ))

(defun run-test-token ()
  (interactive)
  (run-test-interp-token (test-interp-token token whitespace "[[:blank:]]+")))

(pp (macroexpand
      (test-interp-token token whitespace "[[:blank:]]")))

;;----------------------------------------------------------------------
;; test tokens
;;----------------------------------------------------------------------

;; eval this, then eval various tests.

(parser-production-function

(parser-compile test-parser
  (token whitespace "[[:blank:]]+")
  (token word "[[:alpha:]]+"))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+") (token whitespace "[[:blank:]]+")))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+") (token whitespace "[[:blank:]]+")))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+" parser-token-string) (token whitespace "[[:blank:]]+")))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+" parser-token-string) (token whitespace "[[:blank:]]+" null)))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+" parser-token-string) (?? (token whitespace "[[:blank:]]+" null))))

(parser-compile test-parser
  (+ (token word "[[:alpha:]]+" parser-token-string) (token whitespace "[[:blank:]]+" null)))


parser foo bar baz||

(parser-compile test-parser
  (define
    (name bingo (or
                  (token whitespace "[[:blank:]]+")
                  (token word "[[:alpha:]]+"))))
  bingo)

(parser-trace-list test-trace
  (whitespace t)
  (word t)
  (start t))

(parser-compile test-parser
  (define
    (name foo (or
                (token whitespace "[[:blank:]]+")
                (token word "[[:alpha:]]+"))))
  foo)

(parser-compile test-parser
  (and indented (token whitespace "[[:blank:]]+") (token word "[[:alpha:]]+"))
  (and inverted word whitespace))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+"))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+" (lambda ( start stop ) (message "bar by far"))))

(parser-compile test-parser
  (token whitespace "[[:blank:]]+" bingo))

;;----------------------------------------------------------------------
;; test productions
;;----------------------------------------------------------------------

(parser-compile test-parser
  (token whitespace "[[:blank:]]+")
  (token word "[[:alpha:]]+"))

(parser-compile test-parser
  (and indented (token whitespace "[[:blank:]]+") (token word "[[:alpha:]]+"))
  (and inverted word whitespace))

foo bar baz

;;----------------------------------------------------------------------
;; experimental
;;----------------------------------------------------------------------

now create a spiffy function that walks the AST tree for you.

something like start/indented/whitespace
or start/indented

this should lay the grounds for verifying wether the AST is generated as expected,
with parser-walk defining a canonical traversal.

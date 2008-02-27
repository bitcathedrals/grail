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

(defun parser-sugar-dump ( &rest instructions )
  (lexical-let
    ((review-buffer (get-buffer-create "generated")))

    (with-current-buffer review-buffer
      (erase-buffer)

      (insert
        (pp-to-string
          (let
            ((parser-semantic-sugar (parser-create-sugar-table)))
            (parser-sugar-semantics instructions)))))

    (pop-to-buffer review-buffer t)))

;; make it read-only after the wipe and insert, make it elisp with highlighting.
;; then it might be worthy as a utility function.
(defun parser-semantic-dump ( form )
  "dump the code generated by the semantic compiler"
    (condition-case diagnostic
      (let
        ((parser-compile-trace (get-buffer-create "parser-compile-trace"))

         (parser-semantic-sugar (parser-create-sugar-table))
         (match-table (parser-make-symbol-table)))

        (with-current-buffer parser-compile-trace
          (erase-buffer))

        (parser-semantic-interpreter-terminate
          (parser-translate-grammar-form form))

        (pop-to-buffer parser-compile-trace t))

      (parser-syntatic-error
        (message "Syntactic error in grammar or semantics %s" (cdr diagnostic)))

      (parser-semantic-error
        (message "Invalid semantics detected %s" (cdr diagnostic)))

      (parser-compile-error
        (message "Internal Parser Compiler error %s" (cdr diagnostic))) ))

;; test the single token/function case

;; gen-logic-branch is a void variable. kill the stale code.
(parser-semantic-dump `(token))

;; check an explicit sequence
(parser-semantic-dump
  `(/relation-or foo bar))

;; check the default sequence
(parser-semantic-dump
  `(foo bar))

;; test the closure case
(parser-semantic-dump
  `(/greedy foo bar))

;; test input effects
(parser-semantic-dump
  `(/input-discard foo))

;; the first branching test.
(parser-semantic-dump
  `(/input-branch foo))

;; test AST discard
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'ast-discard)

;; test AST discard with a branch
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'input-branch
  'ast-discard)

;; test the AST conditional
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'ast-branch)

;; important assumption. The generated parser code assumes a left to right eval
;; with effects settling before the right argument is eval'd. This test
;; should print 8 if this assumption holds, or 10 if this assumption is broken.

;; using this assumption is a real cheat lisp wise, but I don't think the
;; emacs optimizer is going to penalize me for it any time soon.

(setq foo 9)
(cons (setq foo 7) (+ foo 1))

;; test the AST transform.

(parser-semantic-dump
  `(/transform 'ast-tr foo bar))

;; test the AST transform combined with branching

(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'ast-branch
  `(ast-transform 'transform-foo))

;; test the AST transform combined with a node.
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  `(ast-node 'prod-left)
  `(ast-transform 'transform-foo))

;; now for the awesome: node, transform, and branch in one go.
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  `(ast-node 'prod-left)
  `(ast-transform 'transform-foo)
  'ast-branch)

;; simple AST node.
(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  `(ast-node 'prod-left))

;; test both effects branching

(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'ast-branch
  'input-branch)

;; setup a typical looking left production, effects only, not the compilation issues.

(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'ast-branch
  'input-branch
  `(ast-node 'prod-foo))

;;----------------------------------------------------------------------
;; sugar testing
;;----------------------------------------------------------------------
(parser-sugar-dump
  `(production 'foo))

(parser-semantic-dump
  `(call 'foo)
  `(call 'bar)
  'greedy
  `(production 'foo))

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

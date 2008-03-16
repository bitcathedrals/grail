(require 'closure)

(closure-define test-closure
  (red "hot")
  (blue "cold")
  (symbol 'foo))

(pp-closure (closure-create test-closure))
(setq foo-closure (closure-create test-closure))

(pp-closure foo-closure)

(save-lexical-closure foo-closure
  (message "red is %s" red)
  (message "blue is %s" blue))

(save-lexical-closure foo-closure
  (setq red "flaming")
  (setq blue "ice"))

(use-dynamic-closure
  (test-closure foo-closure)

  (message "red is %s" red)
  (message "blue is %s" blue))


(use-dynamic-closure-with
  (test-closure foo-closure)

  ((foo  "foo!")
   (bar  "bar!"))

   (message "red is %s" red)
   (message "blue is %s" blue)

   (message "foo is %s" foo)
   (message "bar is %s" bar))

;; -*-no-byte-compile: t; -*-

;;
;; tool-wrapper.el
;;

(defun tool-wrapper/invoke-binding ( binding )
  (if (commandp binding)
    (let
      (( current-prefix-arg nil ))

      (call-interactively binding))
    (funcall binding) ))

(defmacro tool-wrapper/wrap-cmd ( wrap-stem wrap-name wrap-doc missing-msg )
  (lexical-let
    ((wrap-fn    (intern (concat wrap-stem wrap-name)) )
     (bind-sym   (intern (concat wrap-stem wrap-name "-binding"))) )

    `(defun ,wrap-fn ()
       ,wrap-doc
       (interactive)

       (if (boundp ',bind-sym)
         (tool-wrapper/invoke-binding ,bind-sym)
         (message ,missing-msg) )) ) )

(defmacro tool-wrapper/wrap-with-bind ( wrap-stem wrap-name wrap-doc missing-msg )
  (lexical-let
    ((wrap-fn    (intern (concat wrap-stem wrap-name)) )
     (bind-fn    (intern (concat wrap-stem "bind-" wrap-name)))
     (bind-sym   (intern (concat wrap-stem wrap-name "-binding"))) )

    `(progn
       (defun ,bind-fn ( bind-fn-arg )
         (set (make-local-variable ',bind-sym) bind-fn-arg))

       (defun ,wrap-fn ()
         ,wrap-doc
         (interactive)

         (if (boundp ',bind-sym)
           (tool-wrapper/invoke-binding ,bind-sym)
           (message ,missing-msg) )) ) ))

(tool-wrapper/wrap-with-bind "monkey/" "test" "into space" "no rocket")

(tool-wrapper/wrap-cmd "monkey/" "test" "into space" "no rocket")


(provide 'tool-wrapper)

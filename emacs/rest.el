;;
;; rest client
;;
(require 'cl)

(defun rest-buffer ( url )
  (interactive "sURL:")

  (lexical-let
    ((url-buffer nil))

    (setq url-buffer (url-retrieve
                       url
                       (lambda ( &rest args )
                         (pop-to-buffer url-buffer)) )) ))


(custom-key-group "rest" "u" t
  ("g" . rest-buffer))

(provide 'rest)

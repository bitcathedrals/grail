;;----------------------------------------------------------------------
;; lex-cache.el - cache data lexically
;;
;; description:
;;
;; cache a piece of data lexically.
;;----------------------------------------------------------------------
(require 'cl)

;;
;; cache expiration
;;

(defun lex-cache-timestamp ()
  (truncate (time-to-seconds (current-time))))

(defun lex-cache-expired ( timestamp interval )
  (if (> (lex-cache-timestamp) (+ timestamp interval))
    t
    nil))

(defun lex-cache-refresh ( timestamp interval )
  (or
    (not timestamp)
    (lex-cache-expired timestamp interval)) )

(defun lex-cache-minutes ( min )
  (* 60 min))

;;
;; lexical caching creation
;;

(defun lex-cache-bind ( symbol cache-fn )
   (fset symbol cache-fn)
   symbol)

(defun lex-cache-build ( builder interval )
  (lexical-let
    ((builder-fn builder)
     (data nil)
     (interval interval)
     (timestamp nil))

    (lambda ( &optional update-flag )
      (if (or
            update-flag
            (not data)
            (lex-cache-refresh timestamp interval))
        (progn
          (setq
            data (funcall builder-fn)
            timestamp (lex-cache-timestamp))
          data)
        data)) ))

(defmacro lex-cache ( binding interval fn )
  `(lex-cache-bind ',binding
     ,(lex-cache-build
       fn
       interval)) )

(defmacro lex-cache-lambda ( interval fn )
  `(lex-cache-build
     ,fn
     ,interval))

(provide 'lex-cache)

(defun maximize-frame ()
  "toggle maximization the current frame"
  (interactive)
  (cond
    ((eq 'x (window-system))

      (progn
        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
          '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))

        (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
          '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)) ))

    ((eq 'w32 (window-system))
      (w32-send-sys-command 61488))

    ((message "window system %s is not supported by maximize" (symbol-name (window-system)))) ))

(defun fullscreen-frame ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
    '(2 "_NET_WM_STATE_FULLSCREEN" 0)))


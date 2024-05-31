(require 'programming-generic)
(require 'yaml-mode)

(defun configure-modes-configure ()
  "toml-mode-configure

   configure toml mode
  "
  (interactive)

  (programming-mode-generic nil) )

(setq auto-mode-alist (append '(("\\.toml\\'"    . toml-ts-mode)
                                ("\\.yaml\\'"    . yaml-mode)
                                ("\\.yml\\'"     . yaml-mode)) auto-mode-alist))

(add-hook 'yaml-mode-hook 'configure-modes-configure)
(add-hook 'toml-ts-mode 'configure-modes-configure)

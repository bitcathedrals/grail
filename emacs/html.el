(require 'sgml-mode)

(defconst html-mode-config/name "html")

(setq auto-mode-alist (append
                        (if (treesit-language-available-p 'html)
                          '(("\\.html\\'" . html-ts-mode))
                          '(("\\.html\\'" . html-mode)))
                        auto-mode-alist))

(defun html-mode/configuration ()
  (company-mode)
  (setq company-backends (cons 'company-capf company-backends))

  (programming-mode-generic 'html nil html-mode-config/name)

  (dwim-tab-localize-context
    (dwim-tab-make-expander 'dwim-tab/after-word 'company-complete)) )

(add-hook 'html-mode-hook 'html-mode/configuration)

(require 'html-mode)

(setq auto-mode-alist (append
                        (if (treesit-language-available-p 'html)
                          '(("\\.html\\'" . html-ts-mode))
                          '(("\\.html\\'" . html-mode)))
                        auto-mode-alist))


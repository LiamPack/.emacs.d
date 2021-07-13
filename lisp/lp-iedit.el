;; iedit config file

(require 'use-package)

(setq iedit-toggle-key-default (kbd "C-:"))
(use-package iedit
  :straight t
  :init 
  (setq iedit-toggle-key-default (kbd "C-:"))
  :config
  (setq iedit-toggle-key-default (kbd "C-:"))
  :bind (("C-:" . #'iedit-mode)))

(provide 'lp-iedit)

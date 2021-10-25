                                        ; mc tips and tricks!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
(use-package expand-region
  :straight t
  :bind ("C-," . er/expand-region))

(use-package zop-to-char                ; Better zapping
  :straight t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree                  ; Branching undo
  :straight t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package iedit
  :straight t
  :init
  :config
  (setq iedit-toggle-key-default (kbd "C-:"))
  :bind (("C-:" . #'iedit-mode)))

(provide 'lp-mc)

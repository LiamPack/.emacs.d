(require 'use-package)
(use-package hungry-delete
  :disabled t
  :ensure t
  :config
  (global-hungry-delete-mode t)
  (global-set-key (kbd "C-x <deletechar>") 'global-hungry-delete-mode))
(provide 'use-hungry-delete)

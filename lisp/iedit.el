;; iedit config file

(require 'use-package)

(use-package iedit
  :straight t
  :bind (("C-:" . #'iedit-mode)))

(provide 'use-iedit)

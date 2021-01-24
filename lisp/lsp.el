;;; TODO

;;; Language Server Protocol for better function finding and all sorts
;;; of fun stuff
(require 'use-package)
(use-package lsp-mode
  :disabled t
  :diminish lsp-mode
  :straight t
  :hook ((python-mode-hook c-common-mode-hook julia-mode-hook) . lsp))

(use-package lsp-ui
  :straight t
  :diminish lsp-ui-mode)
(use-package lsp-treemacs
  :straight t
  :diminish lsp-treemacs-mode)

(provide 'use-lsp)

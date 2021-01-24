(require 'use-package) ;; flycheck mode is not too bad.
(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :defer t
  :hook
  ((prog-mode-hook . flycheck-mode))
  :config
  ;; Turn flycheck on everywhere
  ;; (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )

(provide 'lp-flycheck)

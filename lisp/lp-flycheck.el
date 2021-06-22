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
  (defvar-local my/flycheck-local-cache nil)

  (defun my/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker my/flycheck-local-cache))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-mypy)))))))))
  )

(provide 'lp-flycheck)

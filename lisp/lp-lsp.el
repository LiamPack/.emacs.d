;;; TODO

;;; Language Server Protocol for better function finding and all sorts
;;; of fun stuff
(require 'use-package)
(use-package lsp-mode
  :diminish lsp-mode
  :straight t
  :hook ((c++-mode-hook python-mode-hook cuda-mode-hook c-common-mode-hook julia-mode-hook) . lsp)
  :bind
  (:map
   lsp-mode-map
   ("C-c y n" . lsp-rename)
   ("C-c y o" . lsp-restart-workspace)
   ("C-c y c" . lsp-disconnect)
   ("C-c f" . lsp-format-region))
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation nil)
  (add-to-list 'lsp-file-watch-ignored "build")

  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (setq lsp-prefer-capf t)

  ;; Increase the amount of data which Emacs reads from the process. The emacs
  ;; default is too low 4k considering that the some of the language server
  ;; responses are in 800k - 3M range. Set to 1MB
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-clients-clangd-args '("-j=4" "--clang-tidy"))
  ;; Use flycheck instead of flymake
  (setq lsp-prefer-flymake nil)

  )


(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :diminish lsp-ui-mode
  :bind
  (:map
   lsp-ui-mode-map
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-?" . lsp-ui-peek-find-references))
  :config
  (setq
   ;; Disable sideline hints
   lsp-ui-imenu-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-doc-use-childframe nil
   ;; Disable imenu
   lsp-ui-imenu-enable nil
   ;; Disable ui-doc (already present in minibuffer)
   lsp-ui-doc-enable nil
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature nil
   ;; lsp-ui-doc-background (doom-color 'base4)
   ;; lsp-ui-doc-border (doom-color 'fg)
   ;; Enable ui-peek
   lsp-ui-peek-enable t
                                        ;lsp-ui-peek-fontify t
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-always-show nil
   lsp-ui-peek-force-fontify nil
   lsp-ui-flycheck-enable nil
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))
  ;; Flycheck
  (setq-default flycheck-disabled-checkers '(c/c++-clang
                                             c/c++-cppcheck c/c++-gcc)) )

(use-package lsp-treemacs
  :straight t
  :diminish lsp-treemacs-mode)

(provide 'lp-lsp)

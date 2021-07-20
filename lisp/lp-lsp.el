;;; TODO

;;; Language Server Protocol for better function finding and all sorts
;;; of fun stuff
(require 'use-package)
(use-package lsp-mode
  :diminish lsp-mode
  :straight t
  :hook (((python-mode-hook cc-mode-hook c-mode-hook c++-mode-hook cuda-mode-hook c-common-mode-hook julia-mode-hook rjsx-mode-hook typescript-mode-hook) . lsp)
         )
  :bind
  (:map
   lsp-mode-map
   ("C-c y n" . lsp-rename)
   ("C-c y o" . lsp-restart-workspace)
   ("C-c y c" . lsp-disconnect)
   ("C-c y a" . lsp-execute-code-action)
   ("C-c f" . lsp-format-region))
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-enable-indentation t)
  (setq read-process-output-max (* 10 1024 1024))
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-print-performance nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-response-timeout 5)
  (setq lsp-eldoc-enable-hover t)

  (add-to-list 'lsp-file-watch-ignored "build")
  (add-to-list 'lsp-file-watch-ignored ".clangd")
  (add-to-list 'lsp-file-watch-ignored "pyc")

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
  (setq-default flycheck-disabled-checkers '(c/c++-clang
                                             c/c++-cppcheck c/c++-gcc))

  ;; NB: only required if you prefer flake8 instead of the default
  ;; send pyls config via lsp-after-initialize-hook -- harmless for
  ;; other servers due to pyls key, but would prefer only sending this
  ;; when pyls gets initialised (:initialize function in
  ;; lsp-define-stdio-client is invoked too early (before server
  ;; start)) -- cpbotha
  ;; (defun lsp-set-cfg ()
  ;;   (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
  ;;     ;; TODO: check lsp--cur-workspace here to decide per server / project
  ;;     (lsp--set-configuration lsp-cfg)))
  ;; (push 'company-lsp company-backends)
  ;; (setq company-lsp-cache-candidates 'auto)
  ;; (setq company-lsp-async t)
  ;; (setq company-lsp-enable-snippet nil)
  ;; (setq company-lsp-enable-recompletion t)
  ;; (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
  )


(use-package lsp-ui
  :straight t
  :disabled
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

  )

(use-package dap-mode
  :disabled
  :straight t
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :config
  (eval-when-compile
    (require 'cl))

  (require 'dap-python)
  (require 'dap-lldb)
  (setq dap-python-debugger 'ptvsd)
  (setq dap-python-terminal nil)
  ;; Eval Buffer with `M-x eval-buffer' to register the newly created template.

  (dap-register-debug-template
   "Python :: Run go-ask-alice --get_alice_scores"
   (list :type "python"
         :request "launch"
         :cwd "/home/packell1/irads/just-ask-alice/src"
         :program "main.py"
         :args ["--get_alice_scores"]
         :name "Python :: Run go-ask-alice --get_alice_scores"))

  )

(use-package lsp-treemacs
  :straight t
  :diminish lsp-treemacs-mode)

(use-package lsp-julia
  :disabled
  :straight t
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5"))


(provide 'lp-lsp)

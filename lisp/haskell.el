;; (require 'use-package)

;; (use-package haskell-mode               ; Haskell major mode
;;   :straight t
;;   :defer t
;;   :bind (:map haskell-mode-map
;;               ("M-." . haskell-mode-jump-to-def-or-tag)
;;               ("C-c m i j" . haskell-navigate-imports)
;;               ("C-c m i s" . haskell-sort-imports)
;;               ("C-c m i a" . haskell-align-imports)
;;               ;; Recommended Haskell Mode bindings, see
;;               ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
;;               )
;;   :config
;;   (setq haskell-tags-on-save t ; Regenerate TAGS on save
;;                  haskell-process-log t  ; Show log for GHCI process
;;                  ;; Remove unused imports and auto-import modules
;;                  haskell-process-suggest-remove-import-lines t
;;                  haskell-process-auto-import-loaded-modules t)

;;   (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
;;   (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

;; (use-package haskell                    ; Interactive Haskell
;;   :ensure haskell-mode
;;   :defer t
;;   :bind (:map haskell-mode-map
;;               ("C-c C-l" . haskell-process-load-file)
;;               ("C-`" . haskell-interactive-bring)
;;               ("C-c C-t" . haskell-process-do-type)
;;               ("C-c C-i" . haskell-process-do-info)
;;               ("C-c C-c" . haskell-process-cabal-build)
;;               ("C-c C-k" . haskell-interactive-mode-clear)
;;               ("C-c c" . haskell-process-cabal)
;;               :map interactive-haskell-mode-map
;;               ("C-c m t" . haskell-mode-show-type-at))
;;   :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

;; (use-package haskell-compile            ; Haskell compilation
;;   :ensure haskell-mode
;;   :defer t
;;   :bind (:map haskell-mode-map
;;               ("C-c m c" . haskell-compile)
;;               ("<f5>" . haskell-compile))
;;   :config
;;   ;; Build with Stack
;;   (setq haskell-compile-cabal-build-command "stack build"))

;; (use-package cabal-mode                 ; Cabal files
;;   :ensure haskell-mode
;;   :defer t
;;   :bind (:map haskell-cabal-mode-map
;;               ("C-`" . haskell-interactive-bring)
;;               ("C-c C-k" . haskell-interactive-mode-clear)
;;               ("C-c C-c" . haskell-process-cabal-build)
;;               ("C-c c" . haskell-process-cabal)))

;; (use-package hindent                    ; Haskell indentation
;;   :straight t
;;   :defer t
;;   :init
;;   (add-hook 'haskell-mode-hook #'hindent-mode)
;;   :config
;;   (setq hindent-style "gibiansky"))

;; (provide 'use-haskell)

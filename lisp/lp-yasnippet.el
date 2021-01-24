(require 'use-package)

;; configuration
(use-package yasnippet
  :straight t
  :functions yas-global-mode yas-expand
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-triggers-in-field t)
  (setq yas-verbosity 0)
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :config
  (yas-reload-all))

;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
;; (use-package company-yasnippet
;;   :straight t
;;   :bind ("C-M-y" . company-yasnippet)
;;   :after (yasnippet))


;; auto yas is pretty damn cool
(use-package auto-yasnippet
  :straight t
  :bind ((  "C-1" . aya-create)
         (  "C-2" . aya-expand)))

(provide 'lp-yasnippet)


                                        ; file management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; clean up permissions and owners, less noisy
(use-package dired
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1)
              (require 'dired-x)
              (dired-dotfiles-toggle)))

  ;; disable ls by default
  (setq dired-use-ls-dired nil))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :diminish recentf-mode
  :config
  (setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15
   ;; Cleanup recent files only when Emacs is idle, but not when the mode
   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
   ;; idles often enough to have the recent files list clean up regularly
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "/itsalltext/"      ; It's all text temp files
                         ;; And all other kinds of boring files
                         )))


(use-package wgrep
  :straight t
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-p" . wgrep-change-to-wgrep-mode)))

(provide 'lp-recentf)

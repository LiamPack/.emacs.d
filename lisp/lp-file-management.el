                                        ; file management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; clean up permissions and owners, less noisy
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            (require 'dired-x)
            (dired-dotfiles-toggle)))

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; disable ls by default
(setq dired-use-ls-dired nil)

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

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  ;; Projectile everywhere obviously
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; handle very large files
(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(use-package wgrep
  :straight t)

(provide 'lp-recentf)

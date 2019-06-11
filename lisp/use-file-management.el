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
  :ensure t
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
                         #'ignoramus-boring-p)))

(use-package projectile
  :ensure t
  :config
  (require 'projectile)

  ;; Projectile everywhere obviously
  (projectile-global-mode)

  (defun lp/search-project-for-symbol-at-point ()
    "Use projectile-ag to search current project for the current symbol."
    (interactive)
    (projectile-ag (projectile-symbol-at-point)))
  (global-set-key  (kbd "C-c v") 'projectile-ag)
  (global-set-key (kbd "C-c C-v") 'lp/search-project-for-symbol-at-point))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 30
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t)
  (global-set-key (kbd "C-c f t") 'neotree-toggle))

;; handle very large files
(use-package vlf
  :ensure t
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

(provide 'use-recentf)

;;; Fringe mode
(lp-emacs-builtin-package 'fringe
  (fringe-mode +1)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries t)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

(lp-emacs-elpa-package 'modus-themes
  (setq modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-org-blocks 'gray-background
	modus-themes-prompts '(italic bold))
  (setq modus-operandi-palette-overrides
	'((cursor red-intense)))
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (define-key global-map (kbd "C-c C-8") #'modus-themes-toggle))

(lp-emacs-elpa-package 'doric-themes
  (define-key global-map (kbd "C-c C-7") #'doric-themes-rotate))
(lp-emacs-elpa-package 'ef-themes
  (define-key global-map (kbd "C-c C-0") #'ef-themes-rotate))
(lp-emacs-elpa-package 'standard-themes
  (define-key global-map (kbd "C-c C-9") #'standard-themes-rotate))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql
	  (cond ((numberp alpha) alpha)
		((numberp (cdr alpha)) (cdr alpha))
		;; Also handle undocumented (<active> <inactive>) form.
		((numberp (cadr alpha)) (cadr alpha)))
	  100)
	 '(92 . 92) '(100 . 100)))))
(global-set-key (kbd "C-c &") 'toggle-transparency)

;; https://madmalik.github.io/mononoki/
;; https://leahneukirchen.org/fonts/
;; bdftopcf, https://thristian.livejournal.com/90017.html
;; https://moritzfuerst.net/projects/smalltalk-type
;; (set-frame-font :font "iosevka comfy" :height 120)
;; (let (
;;       ;; (mono-spaced-font "Greybeard 11px")
;;       ;; (proportionately-spaced-font "Greybeard 11px")
;;       (mono-spaced-font "Iosevka Comfy Wide Motion")
;;       (proportionately-spaced-font "Iosevka Comfy Wide Motion")
;;       ;; (mono-spaced-font "BigBlueTermPlus Nerd Font")
;;       ;; (proportionately-spaced-font "BigBlueTermPlus Nerd Font")

;;       )
;;   (set-face-attribute 'default nil :family mono-spaced-font :height 120)
;;   (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
;;   (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

(lp-emacs-elpa-package 'nerd-icons
  (setq nerd-icons-scale-factor 1.0)
  (setq nerd-icons-font-family "BigBlueTermPlus Nerd Font"))
(lp-emacs-elpa-package 'nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))
(load-theme 'standard-light :no-confirm)

(provide 'lp-themes)

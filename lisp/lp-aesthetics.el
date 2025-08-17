;;; Fringe mode
(lp-emacs-builtin-package 'fringe
  (fringe-mode +1)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries t)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Color designing
;; (lp-emacs-elpa-package 'rainbow-mode
;;   (setq rainbow-ansi-colors nil)
;;   (setq rainbow-x-colors nil))

(lp-emacs-elpa-package 'modus-themes
  (setq modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-org-blocks 'gray-background
	modus-themes-prompts '(italic bold))

  (setq modus-operandi-palette-overrides
	'((bg-mode-line-active bg-red-subtle)
	  (bg-mode-line-inactive bg-ochre)
	  (bg-hl-line bg-red-nuanced)
	  (cursor red-intense)))
  (setq modus-vivendi-palette-overrides
	'((bg-mode-line-active bg-red-subtle)
	  (bg-mode-line-inactive bg-ochre)
	  (bg-hl-line bg-red-nuanced)
	  (cursor fg-clay)))

  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (define-key global-map (kbd "C-c C-8") #'modus-themes-toggle)
  )

(lp-emacs-elpa-package 'doric-themes
  (define-key global-map (kbd "C-c C-7") #'doric-themes-rotate)
  )


(lp-emacs-elpa-package 'ef-themes)

;;; I've grown fond of the default emacs themes
(lp-emacs-elpa-package 'standard-themes
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
	standard-themes-disable-other-themes t
        standard-themes-mixed-fonts nil
        standard-themes-variable-pitch-ui nil
        standard-themes-prompts '(bold italic)

        ;; Accepts a symbol value:
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links '(neutral-underline)
        standard-themes-region '(no-extend intense)

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified):
	)

  (setq standard-dark-palette-overrides
	'((bg-mode-line-active "#303030")
	  (bg-mode-line-inactive "#101010")
	  (cursor red-warmer)
	  (bg-region bg-yellow-intense)))

  (setq standard-light-palette-overrides
	'((bg-mode-line-active bg-alt)
	  (bg-mode-line-inactive bg-dim)
	  (cursor red-warmer)
	  (bg-region bg-yellow-intense))))


 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
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

(let (
      ;; (mono-spaced-font "Iosevka Comfy Wide Motion Fixed")
      ;; (proportionately-spaced-font "Iosevka Comfy Wide Duo")
      (mono-spaced-font "Iosevka Comfy Wide Motion")
      (proportionately-spaced-font "Iosevka Comfy Wide Motion Duo")
      ;; (mono-spaced-font "BigBlueTermPlus Nerd Font")
      ;; (proportionately-spaced-font "BigBlueTermPlus Nerd Font")

      )
  (set-face-attribute 'default nil :family mono-spaced-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))


(lp-emacs-elpa-package 'nerd-icons
  (setq nerd-icons-scale-factor 1.0)
  (setq nerd-icons-font-family "BigBlueTermPlus Nerd Font")
  )

(lp-emacs-elpa-package 'nerd-icons-completion
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(lp-emacs-elpa-package 'nerd-icons-dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))




(load-theme 'doric-dark :no-confirm)

;; (global-set-key (kbd "C-c *") #'(lambda () (interactive)
;; 				  (standard-themes-toggle)
;; 				  (standard-themes-with-colors
;; 				    (set-face-attribute 'denote-faces-title nil
;; 							:foreground fg-main
;; 							:box bg-alt))))

(provide 'lp-aesthetics)

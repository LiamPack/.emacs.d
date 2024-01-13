;;; Fringe mode
(lp-emacs-builtin-package 'fringe
  (fringe-mode +1)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Color designing
;; used to have 'ct but its a melpa package and im sticking to non-gnu
;; and gnu-elpa
(lp-emacs-elpa-package 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

(lp-emacs-git-package 'modus-themes
  "https://github.com/protesilaos/modus-themes.git"
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-org-blocks 'gray-background
	modus-themes-prompts '(italic bold)
	modus-themes-region '(bg-only))

  (setq modus-themes-common-palette-overrides
	'())
  ;; (setq modus-vivendi-palette-overrides
  ;; 	`((fg-region unspecified)
  ;; 	  (fringe unspecified)

  ;; 	  (underline-link border)
  ;;         (underline-link-visited border)
  ;;         (underline-link-symbolic border)

  ;; 	  (bg-region bg-red-subtle) ; try to replace `bg-ochre' with `bg-lavender', `bg-sage'
  ;; 	  (bg-paren-match bg-green-intense)
  ;; 	  (underline-paren-match fg-main)

  ;; 	  (bg-hl-line bg-sage)

  ;; 	  (builtin green)
  ;;         (comment yellow-faint)
  ;;         (constant blue)
  ;;         (fnname green-warmer)
  ;;         (keyword green-cooler)
  ;;         (preprocessor green)
  ;;         (docstring green-faint)
  ;;         (string magenta)
  ;;         (type cyan-warmer)
  ;;         (variable blue-warmer)

  ;; 	  (border-mode-line-active unspecified)
  ;;         (border-mode-line-inactive unspecified)
  ;; 	  (bg-mode-line-active bg-green-subtle)))
  ;; (setq modus-vivendi-deuteranopia-palette-overrides
  ;; 	`((fg-region unspecified)
  ;; 	  (fringe unspecified)

  ;; 	  (underline-link border)
  ;;         (underline-link-visited border)
  ;;         (underline-link-symbolic border)

  ;; 	  (bg-region bg-red-subtle) ; try to replace `bg-ochre' with `bg-lavender', `bg-sage'
  ;; 	  (underline-paren-match fg-main)
  ;; 	  (bg-hl-line bg-lavender)

  ;; 	  (border-mode-line-active unspecified)
  ;;         (border-mode-line-inactive unspecified)))
  )

(lp-emacs-elpa-package 'ef-themes)

;;; I've grown fond of the default emacs themes
(lp-emacs-elpa-package 'standard-themes
  ;; "https://github.com/protesilaos/standard-themes.git"
  ;; Read the doc string of each of those user options.  These are some
  ;; sample values.
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
	standard-themes-disable-other-themes t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t
        standard-themes-prompts '(bold italic)

        standard-themes-mode-line-accented t

        ;; Accepts a symbol value:
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links '(neutral-underline)
        standard-themes-region '(no-extend intense)

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified):
	standard-themes-headings
	      '((1 . (variable-pitch 1.5))
		(2 . (1.3))
		(agenda-date . (1.3))
		(agenda-structure . (variable-pitch light 1.8))
		(t . (1.1))))

  (setq standard-dark-palette-overrides
	'((bg-mode-line-active "#303030")
	  (border-mode-line-active "#303030")

	  (bg-mode-line-inactive "#101010")
	  ;; (border-mode-line-inactive "#101010")

	  (cursor red-warmer)
	  (bg-region bg-yellow-intense)
	  (bg-magenta-intense bg-yellow-intense)
	  ))
  )


(load-theme 'standard-dark :no-confirm)
(set-face-attribute 'default nil :font "Noto Mono" :height 110)

(lp-emacs-elpa-package 'spacious-padding
  (setq spacious-padding-widths
	'(:internal-border-width 14 :right-divider-width 8 :scroll-bar-width 5))
  (spacious-padding-mode +1))

(provide 'lp-aesthetics)
 

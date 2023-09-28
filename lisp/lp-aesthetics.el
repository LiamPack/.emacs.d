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
        standard-themes-mixed-fonts nil
        standard-themes-variable-pitch-ui nil
        standard-themes-mode-line-accented nil

        ;; Accepts a symbol value:
        standard-themes-fringes 'subtle

        ;; The following accept lists of properties
        standard-themes-links '(neutral-underline)
        standard-themes-region '(no-extend intense)
        standard-themes-prompts '(bold italic)

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified):
        standard-themes-headings
        '((0 . (light 1.0))
          (1 . (light 1.0))
          (2 . (light 1.0))
          (3 . (semilight 1.0))
          (4 . (semilight 1.0))
          (5 . (1.0))
          (6 . (1.0))
          (7 . (1.0))
          (t . (1.0))))

  ;; (setq standard-themes-common-palette-overrides '((cursor white)))
  )


(load-theme 'modus-vivendi-tritanopia :no-confirm)
(set-face-attribute 'default nil :font "Noto Sans Mono" :height 110)

(lp-emacs-elpa-package 'spacious-padding
  (setq spacious-padding-widths
	'(:internal-border-width 18 :right-divider-width 10 :scroll-bar-width 8))
  (spacious-padding-mode +1))

(provide 'lp-aesthetics)
 

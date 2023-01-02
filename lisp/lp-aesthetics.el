;;; Color designing
(lp-emacs-elpa-package 'ct)
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
	`((fg-region unspecified)
	  (fringe unspecified)

	  (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)

	  (bg-region bg-red-subtle) ; try to replace `bg-ochre' with `bg-lavender', `bg-sage'
	  (bg-paren-match bg-green-intense)
	  (underline-paren-match fg-main)

	  (bg-hl-line bg-sage)

	  (builtin green)
          (comment yellow-faint)
          (constant blue)
          (fnname green-warmer)
          (keyword green-cooler)
          (preprocessor green)
          (docstring green-faint)
          (string magenta)
          (type cyan-warmer)
          (variable blue-warmer)

	  (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
	  (bg-mode-line-active bg-green-subtle)))

  ;; add some padding to modeline
  (defun my-modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       ;; Add "padding" to the mode lines
       `(mode-line ((,c :underline ,border-mode-line-active
			:overline ,border-mode-line-active
			:box (:line-width 10 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :underline ,border-mode-line-inactive
				 :overline ,border-mode-line-inactive
				 :box (:line-width 10 :color ,bg-mode-line-inactive)))))))

  ;; ESSENTIAL to make the underline move to the bottom of the box:
  (setq x-underline-at-descent-line t)

  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces))

(lp-emacs-git-package 'ef-themes
  "https://github.com/protesilaos/ef-themes.git")

;;; I've grown fond of the default emacs themes
(lp-emacs-git-package 'standard-themes
  "https://github.com/protesilaos/standard-themes.git"
  ;; Read the doc string of each of those user options.  These are some
  ;; sample values.
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts nil
        standard-themes-variable-pitch-ui nil
        standard-themes-mode-line-accented t

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
        '((0 . (light 1.9))
          (1 . (light 1.8))
          (2 . (light 1.7))
          (3 . (semilight 1.6))
          (4 . (semilight 1.5))
          (5 . (1.4))
          (6 . (1.3))
          (7 . (1.2))
          (t . (1.1)))))

;;; Softer themes for that kind of mood
(lp-emacs-git-package 'lambda-themes
  "https://github.com/Lambda-Emacs/lambda-themes.git"
  (setq lambda-themes-set-italic-comments t)
  (setq lambda-themes-set-italic-keywords t)
  (setq lambda-themes-set-variable-pitch t))


(load-theme 'modus-vivendi :no-confirm)
(set-face-attribute 'default nil :height 110)

(provide 'lp-aesthetics)

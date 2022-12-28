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
	modus-themes-preset-overrides-intense))

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


(load-theme 'standard-dark :no-confirm)
(set-face-attribute 'default nil :height 110)

(provide 'lp-aesthetics)

;;; Fringe mode
(lp-emacs-builtin-package 'fringe
  (fringe-mode +1)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries t)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Color designing
;; used to have 'ct but its a melpa package and im sticking to non-gnu
;; and gnu-elpa
(lp-emacs-elpa-package 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

(lp-emacs-elpa-package 'modus-themes
  (setq modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
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
	standard-themes-headings
	      '((1 . (1.5))
		(2 . (1.3))
		(agenda-date . (1.3))
		(agenda-structure . (variable-pitch light 1.8))
		(t . (1.1))))

  (setq standard-dark-palette-overrides
	'((bg-mode-line-active "#303030")
	  (bg-mode-line-inactive "#101010")
	  (cursor red-warmer)
	  (bg-region bg-yellow-intense)))

  (setq standard-light-palette-overrides
	'(;; (bg-mode-line-active "#3F3F3F")
	  ;; (bg-mode-line-inactive "#1F1F1F")
	  ;; (bg-mode-line-active bg-green-subtle)
	  ;; (bg-mode-line-inactive bg-green-nuanced)
	  (bg-mode-line-active bg-alt)
	  (bg-mode-line-inactive bg-dim)
	  (cursor red-warmer)
	  (bg-region bg-yellow-intense))
	)

  ;; (with-eval-after-load 'denote
  ;;   (standard-themes-with-colors
  ;;     (set-face-attribute 'denote-faces-title nil
  ;; 			  :foreground fg-main
  ;; 			  :box bg-alt)))
  )


(lp-emacs-elpa-package 'spacious-padding
  (setq spacious-padding-widths
	'(:internal-border-width 14 :right-divider-width 8 :scroll-bar-width 5))
  (setq spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode 1))

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
          '(85 . 79) '(100 . 100)))))


 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)

(global-set-key (kbd "C-c &") 'toggle-transparency)

;; https://madmalik.github.io/mononoki/
;; https://leahneukirchen.org/fonts/
;; bdftopcf, https://thristian.livejournal.com/90017.html
;; https://moritzfuerst.net/projects/smalltalk-type

(load-theme 'standard-light :no-confirm)
;; (load-theme 'standard-dark :no-confirm)

;; (global-set-key (kbd "C-c *") #'(lambda () (interactive) (standard-themes-toggle)
;; 				  (standard-themes-with-colors
;; 				    (set-face-attribute 'denote-faces-title nil
;; 							:foreground fg-main
;; 							:box bg-alt))))

;; (set-face-attribute 'default nil
;; 		    :font "Smalltalk Sans12"
;; 		    :height 155
;; 		    :weight 'normal
;; 		    :width 'normal)

(set-face-attribute 'default nil
		    :font "Smalltalk Sans12"
		    :height 100
		    :weight 'normal
		    :width 'normal)
;; (set-face-attribute 'variable-pitch nil
;; 		    :font "Cardo"
;; 		    :height 110
;; 		    :weight 'normal
;; 		    :width 'normal)
;; (add-hook 'text-mode-hook #'variable-pitch-mode)


(provide 'lp-aesthetics)

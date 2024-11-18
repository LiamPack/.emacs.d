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
	modus-themes-prompts '(italic bold)
	modus-themes-region '(bg-only)))

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
	'((bg-mode-line-active bg-alt)
	  (bg-mode-line-inactive bg-dim)
	  (cursor red-warmer)
	  (bg-region bg-yellow-intense)))
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
          '(92 . 92) '(100 . 100)))))
(global-set-key (kbd "C-c &") 'toggle-transparency)

;; https://madmalik.github.io/mononoki/
;; https://leahneukirchen.org/fonts/
;; bdftopcf, https://thristian.livejournal.com/90017.html
;; https://moritzfuerst.net/projects/smalltalk-type

;; (load-theme 'standard-light :no-confirm)
;; (load-theme 'standard-dark :no-confirm)
(load-theme 'modus-vivendi-tritanopia :no-confirm)

(global-set-key (kbd "C-c *") #'standard-themes-toggle)

(provide 'lp-aesthetics)

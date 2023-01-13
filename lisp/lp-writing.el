(lp-emacs-builtin-package 'text-mode
  (add-hook 'text-mode-hook
            #'(lambda ()
                (interactive)
                (setq-local paragraph-start "\\|\\*\\| *-\\| *[1-9]\\.\\|[ 	]*$"))))

(lp-emacs-builtin-package 'outline
  (setq outline-regexp "^= .+ =\n=+$\\|^- .+ -\n-+$\\|^[*]+"))

(lp-emacs-elpa-package 'markdown-mode)

(lp-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t))

(lp-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((text-mode .  "") ; just use the page-break to make things
			      ; simple
          (org-mode . "^\\*+ +")
          (t . ,(or outline-regexp logos--page-delimiter))))

  (setq-default logos-hide-mode-line t
                logos-hide-buffer-boundaries nil
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-scroll-lock t
                logos-olivetti t)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim) ; C-x ]
    (define-key map [remap backward-page] #'logos-backward-page-dwim) ; C-x [
    (define-key map (kbd "M-^") #'logos-focus-mode)
    ))

;;; style checking and evaluation
(lp-emacs-elpa-package 'smog)
(lp-emacs-elpa-package 'writegood-mode)


;;; spelling and spellchecking
(lp-emacs-builtin-package 'ispell
  (setq ispell-dictionary "english")
  (setq ispell-silently-savep t))

(lp-emacs-builtin-package 'flyspell
  ;; :diminish flyspell-mode
  (dolist (mode-hook '(org-mode-hook markdown-mode-hook))
    (add-hook mode-hook #'flyspell-mode))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (define-key flyspell-mode-map (kbd "C-;") nil) ;; gets in the way of things.
  (define-key flyspell-mode-map (kbd "C-M-i") nil) ;; gets in the way of things.
  )
(provide 'lp-writing)

(lp-emacs-builtin-package 'text-mode
  (add-hook 'text-mode-hook
            #'(lambda ()
                (interactive)
                (setq-local paragraph-start "\\|\\*\\| *-\\| *[1-9]\\.\\|[ 	]*$"))))

(lp-emacs-builtin-package 'outline
  (setq outline-regexp "^= .+ =\n=+$\\|^- .+ -\n-+$\\|^[*]+"))

(lp-emacs-elpa-package 'markdown-mode)

(lp-emacs-elpa-package 'denote
  (setq denote-directory "~/Dropbox/denotes/")
  (setq denote-allow-multi-word-keywords t)
  (setq denote-known-keywords '("emacs" "school" "food" "programming"
                                "personal"
                                "games" "philosophy" "work" "exercise" "bouldering" "journal"))

  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'text)

  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  (let ((map global-map))
    (define-key map (kbd "C-c f j") #'denote-create-note-in-subdirectory) ; our custom command
    (define-key map (kbd "C-c f n") #'denote)
    (define-key map (kbd "C-c f N") #'denote-type)
    (define-key map (kbd "C-c f d") #'(lambda ()
                                        (interactive)
                                        (dired (denote-directory))))

    (define-key map (kbd "C-c f i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c f I") #'denote-link-add-links)
    (define-key map (kbd "C-c f l") #'denote-link-find-file) ; "list" links
    (define-key map (kbd "C-c f b") #'denote-link-backlinks)
    (define-key map (kbd "C-c f r") #'denote-rename-file)
    (define-key map (kbd "C-c f R") #'denote-rename-file-using-front-matter)))

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

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

  
  (setq lp--journal-date-format "%A %e %B %Y") ; format like Tuesday 14 June 2022
  (defun lp-denote-pop-journal ()
    (interactive)
    (let ((date (format-time-string lp--journal-date-format))
	  (journal-dir (concat denote-directory "notebook/journal/")))
      (if-let* ((fns (directory-files
		      journal-dir
		      t
		      (concat ".*" (denote-sluggify (downcase date)) ".*")))
		(switch-to-buffer-obey-display-actions t))
	(progn
	  (find-file-noselect (car fns))
	  (switch-to-buffer (file-name-nondirectory (car fns))))
      (denote date '("journal") 'text journal-dir))))

  (let ((map global-map))
    (define-key map (kbd "C-c f j") #'denote-create-note-in-subdirectory) ; our custom command
    (define-key map (kbd "C-c C-o") #'lp-denote-pop-journal)
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
    (define-key map (kbd "C-c f R") #'denote-rename-file-using-front-matter))

  (add-to-list 'display-buffer-alist
	       '("\\.*\\(_journal\\|_monthly\\).*.txt\\.*"
		 (display-buffer-below-selected)
		 (window-height 0.3)))
  )

(provide 'lp-denote)

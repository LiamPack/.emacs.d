(lp-emacs-elpa-package 'denote
  (require 'denote-sort)
  (require 'denote-silo-extras)

  (setq denote-directory "~/dropbox/denotes/")
  (setq denote-excluded-directories-regexp "/_.*/")
  (setq denote-allow-multi-word-keywords t)
  (setq denote-known-keywords '("notes" "philosophy" "pedagogy" "meta" "list"
				"research" "exposition" "unfinished"))

  ;; (setq denote-known-keywords '("emacs" "school" "food" "programming"
  ;;                               "personal"
  ;;                               "games" "philosophy" "work" "exercise" "bouldering" "journal"))

  (setq denote-infer-keywords nil)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'org)
  (setq denote-prompts '(signature title keywords file-type))

  (denote-rename-buffer-mode 1)
  (setq denote-rename-buffer-format "[D] %s = %>25t")


  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (require 'denote-journal-extras)
  (setq denote-journal-extras-keyword "journal")
  (setq denote-journal-extras-directory "journal")

  (setq lp--journal-date-format "%A %e %B %Y") ; format like Tuesday 14 June 2022
  (setq lp--monthly-date-format "%b %Y")

  (defun lp--deonte-pop-journal ()
    (interactive)
    (denote-journal-extras-new-or-existing-entry (format-time-string lp--journal-date-format)))

  (defun lp--denote-rename-fn (file)
    (let ((type (denote-filetype-heuristics file)))
      (string-trim
     (format-spec denote-rename-buffer-format
                  (list (cons ?t (denote-retrieve-title-value file type))
                        (cons ?i (denote-retrieve-filename-identifier file))
                        (cons ?d (denote-retrieve-filename-identifier file))
                        (cons ?s (denote-retrieve-filename-signature file))
                        (cons ?k (denote-retrieve-keywords-value-as-string file type))
                        (cons ?% "%"))
                  'delete))))

  (defun lp--denote-pop-monthly ()
    (interactive)
    (let ((date (format-time-string lp--monthly-date-format))
	  (monthly-dir (concat denote-directory "monthlies/")))
      (if-let* ((fns (directory-files
		      monthly-dir
		      t
		      (concat ".*" (denote-sluggify (downcase date)) ".*")))
		(switch-to-buffer-obey-display-actions t)
		(monthly-name  (lp--denote-rename-fn (car fns))))
	(cond
	 ;; need this "window" since it should pop only if displaying
	 ((get-buffer-window monthly-name)
	  (delete-window (get-buffer-window monthly-name)))
	 (t (find-file-noselect (car fns))
	    (switch-to-buffer monthly-name)))
      (denote date '("monthly") 'org monthly-dir))))

  (let ((map global-map))
    (define-key map (kbd "C-c f j") #'denote-subdirectory) ; our custom command
    (define-key map (kbd "C-c C-o") #'lp--denote-pop-journal)
    (define-key map (kbd "C-c C-m") #'lp--denote-pop-monthly)
    (define-key map (kbd "C-c f n") #'denote)
    (define-key map (kbd "C-c f d") #'(lambda ()
                                        (interactive)
                                        (dired (denote-directory))))
    (define-key map (kbd "C-c f i") #'denote-link-or-create) ; "insert" mnemonic
    (define-key map (kbd "C-c f I") #'denote-link-add-links)
    (define-key map (kbd "C-c f l") #'denote-link-find-file) ; "list" links
    (define-key map (kbd "C-c f b") #'denote-link-backlinks)
    (define-key map (kbd "C-c f r") #'denote-rename-file)
    (define-key map (kbd "C-c f R") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "C-c f s") #'denote-sort-dired))

  (add-to-list 'display-buffer-alist
	       '("\[D\].*\\(2022\\|2023\\|2024\\|journal\\).*"
		 (display-buffer-below-selected)
		 (window-height 0.3))))
;; (lp-emacs-elpa-package 'consult-notes
;;   (consult-notes-denote-mode +1))

(provide 'lp-denote)

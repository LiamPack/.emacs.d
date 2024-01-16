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
  (setq denote-file-type 'text)
  (setq denote-prompts '(signature title keywords file-type))

  (denote-rename-buffer-mode 1)
  (setq denote-rename-buffer-format "[D] %s = %>25t")


  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (require 'denote-journal-extras)
  (setq denote-journal-extras-keyword "journal")
  (setq denote-journal-extras-title-format 'day-date-month-year)
  (setq lp--monthly-date-format "%b %Y")

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
    (define-key map (kbd "C-c C-o") #'denote-journal-extras-new-or-existing-entry)
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

;; https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html
;; https://kaorahi.github.io/howm/README.html
(lp-emacs-elpa-package 'howm
  ;; Directory configuration
  (setq howm-home-directory "~/dropbox/denotes/_howm/")
  (setq howm-directory "~/dropbox/denotes/_howm/")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y%m%dT%H%M%S.txt")

  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  ;; counsel-rg for howm
  (defun howm-list--counsel-rg (match)
    (if (string= match "")
	(howm-list-all)
      (if (or (null ivy--old-cands)
	      (equal ivy--old-cands '("No matches found")))
          (message "No match")
	(let ((howm-view-use-grep
	       #'(lambda (str file-list &optional fixed-p force-case-fold)
                   (mapcar
                    (lambda (cand)
		      (if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                          (let ((file (match-string-no-properties 1 cand))
				(line (match-string-no-properties 2 cand))
				(match-line (match-string-no-properties 3 cand)))
                            (list (expand-file-name file howm-directory)
                                  (string-to-number line)
                                  match-line))))
                    ivy--old-cands))))
          (howm-search ivy--old-re t)
          (riffle-set-place
	   (1+ (cl-position match ivy--old-cands :test 'string=)))))))

  (defun howm-counsel-rg ()
    "Interactively grep for a string in your howm notes using rg."
    (interactive)
    (let ((default-directory howm-directory)
          (counsel-ag-base-command counsel-rg-base-command)
          (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
      (ivy-read "Search all (rg): "
		#'counsel-ag-function
		:dynamic-collection t
		:keymap counsel-ag-map
		:action #'howm-list--counsel-rg
		:require-match t
		:caller 'counsel-rg)))

  (define-key global-map (concat howm-prefix "r") 'howm-counsel-rg)

  ;; Default recent to sorting by mtime
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  ;; Default all to sorting by creation, newest first
  (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

  ;; Rename buffers to their title
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)

  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)

  ;; zotero://
  (add-to-list 'action-lock-default-rules
               (list "\\<zotero://\\S +" (lambda (&optional dummy)
                                           (browse-url (match-string-no-properties 0)))))
  ;; @bibtex
  (add-to-list 'action-lock-default-rules
               (list "\\s-\\(@\\([a-zA-Z0-9:-]+\\)\\)\\>"
                     (lambda (&optional dummy)
                       (browse-url (concat "zotero://select/items/bbt:"
                                           (match-string-no-properties 2))))
                     1))

  )

(provide 'lp-denote)

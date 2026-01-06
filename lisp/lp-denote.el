(lp-emacs-elpa-package 'denote

  (setq denote-directory "~/dropbox/denotes/")
  (setq denote-excluded-directories-regexp "/_.*/")  
  (setq denote-excluded-files-regexp "/_.*/")  
  (setq denote-allow-multi-word-keywords t)
  ;;; TODO: focus the taxonomy
  (setq denote-known-keywords '("meta" "personal" "note" "source" "project" "daily"

				"list" "list>meeting"
				"misc>wallpaper"
				"research" "research>book" "research>paper"

				"math" "math>prob" "math>pde" "math>analysis" "math>topology" "math>functional"
				"math>prob>percolation"

				"media" "media>book" "media>anime"
				"writing" "writing>philosophy" "writing>personal" "writing>literature"

				"cooking" "cooking>recipe"
				"tech"))
  (setq denote-file-name-components-order '(title keywords signature identifier))
  (setq denote-infer-keywords nil)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'text)
  (setq denote-prompts '(signature title keywords file-type))
  (setq denote-link-description-format "%t")

  (denote-rename-buffer-mode -1)
  (setq denote-rename-buffer-format "[D] %s = %>25t")
  (setq denote-buffer-has-backlinks-string " (<--->)")
  (setq denote-backlinks-show-context t)
  (setq denote-org-store-link-to-heading t)


  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'find-file-hook #'denote-fontify-links-mode)

  (add-to-list 'display-buffer-alist
	       '("\[D\].*\\(2022\\|2023\\|2024\\|2025\\|2026\\|journal\\).*"
		 (display-buffer-below-selected)
		 (window-height 0.3)))
  
  ;;;; recurring notes
  ;;; TODO: potential here. (phil, kihoon, pierre, digestion, LIST, X-seminar).
  (defvar my-denote-colleagues '("phil" "kihoon" "digestion" "incubator" "misc-incubator")
    "List of names I collaborate with.
There is at least one file in the variable `denote-directory' that has
the name of this person.")

  (defvar my-denote-colleagues-prompt-history nil
    "Minibuffer history for `my-denote-colleagues-new-meeting'.")

  (defun my-denote-colleagues-prompt ()
    "Prompt with completion for a name among `my-denote-colleagues'.
Use the last input as the default value."
    (let ((default-value (car my-denote-colleagues-prompt-history)))
      (completing-read
       (format-prompt "New meeting with COLLEAGUE" default-value)
       my-denote-colleagues
       nil :require-match nil
       'my-denote-colleagues-prompt-history
       default-value)))

  (defun my-denote-colleagues-get-file (name)
    "Find file in variable `denote-directory' for NAME colleague.
If there are more than one files, prompt with completion for one among
them.

NAME is one among `my-denote-colleagues'."
    (if-let ((files (denote-directory-files (format "%s.*_list" name)))
             (length-of-files (length files)))
	(cond
	 ((= length-of-files 1)
          (car files))
	 ((> length-of-files 1)
          (completing-read "Select a file: " files nil :require-match)))
      (user-error "No files for colleague with name `%s'" name)))

  (defun my-denote-colleagues-new-meeting ()
    "Prompt for the name of a colleague and insert a timestamped heading therein.
The name of a colleague corresponds to at least one file in the variable
`denote-directory'.  In case there are multiple files, prompt to choose
one among them and operate therein.

Names are defined in `my-denote-colleagues'."
    (declare (interactive-only t))
    (interactive)
    (let* ((name (my-denote-colleagues-prompt))
           (file (my-denote-colleagues-get-file name))
           (time (format-time-string "%F %a")))  ; remove %R if you do not want the time
      (with-current-buffer (find-file file)
	(goto-char (point-max))
	;; Here I am assuming we are in `org-mode', hence the leading
	;; asterisk for the heading.  Adapt accordingly.
	(insert (format "* [%s]\n\n" time)))))

;;;; templates
  ;;; TODO: tune. (paper, thought, list, snippet).

  ;;;; Luhman signature sorting
  (defun my-denote--split-luhman-sig (signature)
    "Split numbers and letters in Luhmann-style SIGNATURE string."
    (replace-regexp-in-string
     "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
     (replace-regexp-in-string
      "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
      signature)))

  (defun my-denote--pad-sig (signature)
    "Create a new signature with padded spaces for all components"
    (combine-and-quote-strings
     (mapcar
      (lambda (x)
	(string-pad x 5 32 t))
      (split-string (my-denote--split-luhman-sig signature) "=" t))
     "="))

  (defun my-denote-sort-for-signatures (sig1 sig2)
    "Return non-nil if SIG1 is smaller that SIG2.
Perform the comparison with `string<'."
    (string< (my-denote--pad-sig sig1) (my-denote--pad-sig sig2)))

  ;; Change the sorting function only when we sort by signature.
  (setq denote-sort-signature-comparison-function #'my-denote-sort-for-signatures)


  (let ((map global-map))
    (define-key map (kbd "C-c f j") #'denote-subdirectory) ; our custom command
    (define-key map (kbd "C-c f n") #'denote)
    (define-key map (kbd "C-c f m") #'my-denote-colleagues-new-meeting)
    (define-key map (kbd "C-c f d") #'(lambda ()
                                        (interactive)
                                        (dired (denote-directory))))
    (define-key map (kbd "C-c f i") #'denote-link-or-create) ; "insert" mnemonic
    (define-key map (kbd "C-c f I") #'denote-add-links)
    (define-key map (kbd "C-c f l") #'denote-find-link)
    (define-key map (kbd "C-c f b") #'denote-find-backlink)
    (define-key map (kbd "C-c f r") #'denote-rename-file)
    (define-key map (kbd "C-c f R") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "C-c f s") #'denote-sort-dired)))

(lp-emacs-elpa-package 'denote-journal
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year)
  (add-hook 'calendar-mode-hook #'denote-journal-calendar-mode)

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

  ;;; monthlies attempt
  (setq lp--monthly-date-format "%b %Y")
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


  (define-key global-map (kbd "C-c C-m") #'lp--denote-pop-monthly)
  (define-key global-map (kbd "C-c C-o") #'denote-journal-new-or-existing-entry)

  )

(lp-emacs-elpa-package 'denote-silo
  ;; TODO
  )

(provide 'lp-denote)

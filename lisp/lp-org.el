(lp-emacs-builtin-package 'org
  ;;; general
  (setq org-directory "~/dropbox/denotes/")
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-startup-folded t)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-cycle-separator-lines 0)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-hide-emphasis-markers nil)

  ;;; TODOs and refiles -- courtesy of some of prot's configuration of course
  (setq org-refile-targets
        `((,(directory-files org-directory t ".*.org") . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-todo-keyword-faces
        '(("WAIT" . '(bold org-todo))
          ("MAYBE" . '(shadow org-todo))
          ("CANCEL" . '(bold org-done))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related '(latex entities)) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  ;;; tags
  (setq org-tag-alist
        '(("personal")
	  ("work")
	  ("physics")
	  ("cs")
	  ("aiml")
	  ("statistics")
	  ("scifi")
	  ("meeting")
          ("emacs")
          ("politics")
          ("economics")
          ("philosophy")
	  ("cogsci")
          ("book")
	  ("site")
	  ("tv")
          ("essay")
	  ("paper")
          ("website")))

  (setq org-auto-align-tags t)
  (setq org-tags-column 80)

  ;;; logs
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

  ;;; org and tex
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil) ; not a fan of hidden characters
  (setq org-indirect-buffer-display #'current-window)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)

  ;;; org-src and babel
  ;; NOTE: If this isn't working, make sure to delete or
  ;; byte-recompile the /elpa/org/.. directory.
  ;; enable language compiles
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (shell . t)
     (julia . t)))

  (setq org-src-window-setup 'plain) ;; let display-buffer handle it
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

  (setq org-list-allow-alphabetical t)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))


  (setq org-use-sub-superscripts nil)   ; not a big fan of the ambiguity
  (setq org-insert-heading-respect-content t)

  ;; Auto wrap paragraphs in some modes (auto-fill-mode)
  ;; (add-hook 'text-mode-hook #'turn-on-auto-fill)
  ;; (add-hook 'org-mode-hook #'turn-on-auto-fill)

  ;;; capture
  (setq lp--tasks-file (car (directory-files org-directory t ".*tasks.*org")))
  (setq lp--to-read-file (car (directory-files org-directory t ".*to-read.*org")))
  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline lp--tasks-file "Tasks")
           ,(concat "* TODO %^{Title} %^g\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l")
           :empty-lines-after 1)
          ("c" "Clock in to a task" entry
           (file+headline lp--tasks-file "Clocked tasks")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %T\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                    ":END:\n\n"
                    "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines-after 1)
          ("m" "Memorandum of conversation" entry
           (file+headline lp--tasks-flie "Tasks")
           ,(concat "* Memorandum of conversation with %^{Person}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?")
           :empty-lines-after 1)
          ("t" "Task with a due date" entry
           (file+headline lp--tasks-file "Tasks")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
	  ("r" "New to-read entry" entry
	   (file+headline lp--to-read-file "inbox")
	   ,(concat "* TODO [#A] %^{Title} %^g\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%x\n\n%a\n\n%?")
	   :empty-lines-after 1)))

  ;;; archiving
  (setq org-archive-skip-archived-trees t)
  (setq org-archive-location "::* Archive")
  (setq org-agenda-skip-archived-trees t)
  (setq org-archive-mark-done "DONE")
  (setq org-archive-reversed-order t)
  (setq org-archive-save-context-info '(time file ltags itags todo category olpath))
  (setq org-archive-stamp-time t)
  (setq org-archive-subtree-add-inherited-tags t)
  (setq org-columns-skip-archived-trees t)
  (setq org-cycle-open-archived-trees nil)
  (setq org-loop-over-headlines-in-active-region t)
  (setq org-sparse-tree-open-archived-trees t) ; not sure about this
					; yet...

  ;;; agenda
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-file-regexp ".*tasks.*\.org")
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

N  (setq org-agenda-format-date "%A %-e %B %Y")
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 7)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)

  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-time-leading-zero t)

  (setq org-agenda-start-with-follow-mode nil) ; Press F! it'll pop up
					; an indirect buffer
  (setq org-agenda-follow-indirect t)

;;; Use org-agenda for viewing diary entries
  (setq org-agenda-add-entry-text-descriptive-links t)
  (setq org-agenda-add-entry-text-maxlines 0) ; see
					; entry-text-maxlines
					; below
  (setq org-agenda-auto-exclude-function nil)

  (setq org-agenda-bulk-custom-functions nil) ; there's potential here
  (setq org-agenda-bulk-mark-char ?*)
  (setq org-agenda-persistent-marks nil) 
  (setq org-agenda-persistent-filter nil)


  (setq org-agenda-block-separator ?=)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-confirm-kill nil)

  (setq org-agenda-diary-file 'diary-file) ; for inserting diary
					; entries from agenda
  (setq org-agenda-include-diary t)
  (setq org-agenda-insert-diary-strategy 'date-tree-last)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-agenda-insert-diary-exact-time t)

  ;;; viewing gagenda entries
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "        > ")
  (setq org-agenda-entry-text-maxlines 6) ; press E!, it'll sohw the lines.

  (setq org-agenda-ignore-properties nil)
  (setq org-agenda-inhibit-startup nil)

  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4
  (setq org-agenda-restriction-lock-highlight-subtree t) ;honestly idk
					;what this
					;does yet
  (setq org-agenda-skip-unavailable-files t) ; just skip it man.
  (setq org-agenda-timegrid-use-ampm nil)

  ;;; agenda line format
  (setq org-agenda-tags-column 80)
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-scheduled-leaders
	'(" Scheduled: " " Sched.%2dx: "))
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-deadline-leaders
        '(" Deadline:  " "In %3d d.: " "%2d d. ago: "))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))

;;; agenda time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))

;;; agenda column
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)

  (setq lp--custom-agenda
	`((tags-todo "*"
		     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
		       `(org-agenda-skip-entry-if
			'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
		      (org-agenda-block-separator ?=)
                      (org-agenda-overriding-header "Basic tasks to review\n")))
	  (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-span 1)
                      (org-agenda-show-all-dates nil)
                      (org-scheduled-past-days 365)
                      ;; Excludes today's scheduled items
                      (org-scheduled-delay-days 1)
                      (org-agenda-entry-types '(:scheduled))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "")
                      (org-agenda-overriding-header "\nPending scheduled tasks")))
	  (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
		      (org-agenda-block-separator nil)
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's agenda\n")))
	  (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
		      (org-agenda-block-separator nil)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext three days\n")))
	  (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))

  
  (setq lp--custom-toreads
 	`((tags-todo "*" ;; "+sites+blogs+textbooks+utils+papers"
		     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
		       `(org-agenda-skip-entry-if
			 'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-overriding-header "to-reads\n")))))

  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
	   ,lp--custom-agenda
	   ((org-agenda-fontify-priorities nil)
	    (org-agenda-dim-blocked-tasks nil)))
	  ("r" "to-read articles"
	   ,lp--custom-toreads
	   ((org-agenda-with-colors nil)
	    (org-agenda-fontify-priorities nil)
	    (org-agenda-prefix-format "%t %s")
	    (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
	    (org-agenda-remove-tags t)
	    (org-agenda-file-regexp ".*to-read.*\.org")))))
  
;;;;; Agenda habits
  (require 'org-habit)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)
  

  ;; org capture templates
  ;; org directory location
  ;; org display agenda preferences
  ;; binds
  ;; (define-key global-map (kbd "C-c A") (lambda () (interactive) (org-agenda nil "A")))
  ;; (define-key global-map (kbd "C-c r") (lambda () (interactive) (org-agenda nil "r")))

  (define-key text-mode-map (kbd "C-c q") #'auto-fill-mode)
  (define-key global-map (kbd "C-c l") #'org-store-link)
  (define-key global-map (kbd "C-c C-l") #'org-insert-link-global)
  (let ((map global-map))
    (define-key map (kbd "C-c a") #'org-agenda)
    (define-key map (kbd "C-c c") #'org-capture)
    (define-key map (kbd "C-c l") #'org-store-link)
    (define-key map (kbd "C-c o") #'org-open-at-point-global))
  (let ((map org-mode-map))
    ;; I don't like that Org binds one zillion keys, so if I want one
    ;; for something more important, I disable it from here.
    (define-key map (kbd "C-'") nil)
    (define-key map (kbd "C-,") nil)
    (define-key map (kbd "M-;") nil)
    (define-key map (kbd "<C-return>") nil)
    (define-key map (kbd "<C-S-return>") nil)
    (define-key map (kbd "C-M-S-<right>") nil)
    (define-key map (kbd "C-M-S-<left>") nil)
    (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
    (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display)))


(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
		 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(provide 'lp-org)

(require 'use-package)
                                        ; org-mode
                                        ; TODO speed-keys?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :straight t
  :diminish org-bullets-mode
  :hook ((org-mode-hook . org-bullets-mode-hook))
  :config
  (setq org-ellipsis "⤵"))

(use-package cdlatex
  :straight t
  :diminish org-cdlatex-mode
  :hook (org-modeook . org-cdlatex-mode))

(use-package org
  :straight t
  :bind (("\C-cl" . org-store-link)
         ("\C-cb" . org-iswitchb))
  :config
  (require 'org-habit)
  (unbind-key "C-," org-mode-map)       ;expand-region
  (unbind-key "C-'" org-mode-map)       ;avy

  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode)) )

  ;; Some latex stuff in org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq-default org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-listings 'minted)

  ;; Some nice latex pretty-entites!
  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-use-speed-commands t)

  ;; NOTE: If this isn't working, make sure to delete /
  ;; byte-recompile the /elpa/org/.. directory!
  ;; enable language compiles
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     ;;     (sh . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (R . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
    (setq org-src-window-setup 'current-window)
  ;;(setq ob-async-no-async-languages-alist '("ipython"))

;;;  file directory setup
  ;; Org-capture management + Tasks
  (setq org-directory "~/Dropbox/Org/")

  (defun org-file-path (filename)
    "Return absolute address of an org file give its relative name."
    (concat (file-name-as-directory org-directory) filename))
  
  ;; refiling!
  ;; I like to look at pretty much just up to 3 levels of targets
  (setq org-refile-targets '((all-org-files :maxlevel . 3)))

  ;; only look at top level headings. Since org-mode represents
  ;; these as files, this also means that the highest level heading
  ;; will be the first "file" so to speak
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; allow creating new parents on refile
  (setq org-refile-allow-creating-parent-nodes 'confirm)
                                        ; todo stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "DeepSkyBlue1" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  ;; Place tags close to the right-hand side of the window
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)


  ;; Changing a task state is done with C-c C-t KEY
  ;; where KEY is the appropriate fast todo state selection key as defined in org-todo-keywords.
  ;; The setting
  (setq org-use-fast-todo-selection t)

  ;; allows changing todo states with S-left and S-right skipping all of
  ;; the normal processing when entering or leaving a todo state. This
  ;; cycles through the todo states but skips setting timestamps and
  ;; entering notes which is very convenient when all you want to do is
  ;; fix up the status of an entry.
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

                                        ; agenda stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-block-separator 45)
  ;; Check out NOX for stuff
  (require 'calendar)

  (setq-default
   org-agenda-span 'week
   org-agenda-prefix-format '((agenda . "  %?-12t% s")
                              (todo   . "  ")
                              (tags   . "  ")
                              (search . "  "))
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
   org-agenda-dim-blocked-tasks nil
   org-agenda-block-separator ""
                                        ;org-agenda-time-grid '((daily today require-timed) nil "......" "----------------")
   )
  ;; Custom agenda command definitions
  (setq org-tags-match-list-sublevels t)

  ;; Function to skip tag
  ;; From http://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view

  ;; Bind C-c C-x C-s to mark todo as done and archive it
  (defun lp/mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE and archive it"
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))

  (define-key org-mode-map (kbd "C-c C-x C-s") 'lp/mark-done-and-archive)
  (setq org-log-done 'time)   ; also record when the TODO was archived

;;; Org Keybindings
  ;; Useful keybinds
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)

  ;; Auto wrap paragraphs in some modes (auto-fill-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; sometimes i don't want to wrap text though, so we will toggle
  ;; with C-c q
  (global-set-key (kbd "C-c q") 'auto-fill-mode)
)
                                        ; clocking!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ok back to clocking
;;;;;;;;;;;;;;;;;;;;

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
;; (setq org-clock-history-length 23)
;; ;; Resume clocking task on clock-in if the clock is open
;; (setq org-clock-in-resume t)
;; ;; Change tasks to NEXT when clocking in
;; (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; ;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; ;; Save clock data and state changes and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)
;; ;; Clock out when moving task to a done state
;; (setq org-clock-out-when-done t)
;; ;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persist t)
;; ;; Do not prompt to resume an active clock
;; (setq org-clock-persist-query-resume nil)
;; ;; Enable auto clock resolution for finding open clocks
;; (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; ;; Include current clocking task in clock reports
;; (setq org-clock-report-include-clocking-task t)

;; ox-hugo because why not

;; random org stuff now
(setq org-hide-emphasis-markers nil)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(provide 'lp-org)

(require 'use-package)
                                        ; org-mode
                                        ; TODO speed-keys?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  ;; TODO: fix this with the right straight source
  :straight (:type built-in)
  :bind (("\C-cl" . org-store-link))
  :config
  (defun org-clocking-buffer ()
    "Return the buffer where the clock is currently running.
Return nil if no clock is running."
    (marker-buffer org-clock-marker))

  (setq org-priority-highest org-highest-priority)
  (setq org-priority-lowest org-lowest-priority)
  (require 'org-habit)
  (unbind-key "C-," org-mode-map)       ;expand-region
  (unbind-key "C-'" org-mode-map)       ;avy


  ;; Some latex stuff in org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq-default org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-listings 'minted)

  ;; Some nice latex pretty-entites!
  (setq org-startup-with-inline-images t)
  (setq org-startup-folded 'fold)
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
   ;; org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
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
  (setq org-agenda-files '("~/org/inbox.org"))
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (setq org-inbox-directory "~/org/")
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat org-inbox-directory "inbox.org"))
           "* TODO %^{Task} %^g\n%t\n %?")
          ("h" "habit" entry (file ,(concat org-inbox-directory "inbox.org"))
           "* %^{Habit} %^g\n %?")
          ("b" "book" entry (file+headline ,(concat org-inbox-directory "books.org") "backlog")
           "* %? \n  %U")
          ("t" "textbook" entry (file+headline ,(concat org-inbox-directory "books.org") "textbook backlog")
           "* %? \n  %U")))

  ;; Auto wrap paragraphs in some modes (auto-fill-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; sometimes i don't want to wrap text though, so we will toggle
  ;; with C-c q
  (global-set-key (kbd "C-c q") 'auto-fill-mode)


  ;; random org stuff now
  (setq org-hide-emphasis-markers nil)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;;(add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

                                        ; clocking!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ok back to clocking
;;;;;;;;;;;;;;;;;;;;

;; Resume clocking task when emacs is restarted
;; (org-clock-persistence-insinuate)
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

(use-package org-roam
  :straight (:type git :host github
                   :repo "org-roam/org-roam-v1" :branch "master")
  :diminish
  :bind (("\C-c i" . org-roam-insert-immediate)
         ("\C-c j" . org-roam-dailies-find-today)
         ("\C-c o" . org-roam-jump-to-index)
         ("\C-c t" . org-roam-tag-add)
         ("\C-c f" . org-roam-find-file)
         ("\C-c d" . org-roam-dailies-map)
         ("\C-c \C-u" . org-roam-general-map))
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-graph-exclude-matcher '("physics" "textbook" "quote" "paper" "private" "daily" "index" "Index"))
  (org-roam-dailies-directory "daily/")
  (org-roam-db-update-idle-seconds 20)
  :init
  (add-hook 'after-init-hook 'org-roam-mode)
  :config

  (setq org-roam-dailies-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "l") 'org-roam-dailies-find-today)
          (define-key map (kbd "j") 'org-roam-dailies-find-tomorrow)
          (define-key map (kbd "d") 'org-roam-dailies-find-date)
          (define-key map (kbd "k") 'org-roam-dailies-find-yesterday)
          (define-key map (kbd "p") 'org-roam-dailies-find-previous-note)
          (define-key map (kbd "n") 'org-roam-dailies-find-next-note)
          map))
  (setq org-roam-general-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "f") 'org-roam-find-file)
          (define-key map (kbd "c") 'org-roam-capture)
          (define-key map (kbd "i") 'org-roam-insert)
          (define-key map (kbd "r") 'org-roam)
          (define-key map (kbd "I") 'org-roam-insert-immediate)
          (define-key map (kbd "g") 'org-roam-graph)
          (define-key map (kbd "o") 'org-roam-jump-to-index)
          (define-key map (kbd "d") 'deft)
          (define-key map (kbd "t") 'org-roam-tag-add)
          map))
  (require 'org-protocol))

(use-package org-roam-server
  :straight t
  :config
  (setq org-roam-server-host "localhost"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package nroam
  :disabled
  :straight (:type git :host github
                   :branch "master"
                   :repo "NicolasPetton/nroam")
  :after org-roam
  :config
  (add-hook 'org-mode-hook #'nroam-setup-maybe))

(use-package lister
  :disabled
  :straight (:type git :host github
                   :repo "publicimageltd/lister" :branch "main"))

(use-package delve
  :disabled
  :straight (:type git :host github
                   :repo "publicimageltd/delve" :branch "main")
  :config
  (use-package delve-minor-mode
    :config
    (add-hook 'org-mode-hook #'delve-minor-mode-maybe-activate))
  :bind
  (("<f12>" . delve-open-or-select)))

(use-package org-attach-screenshot
  :disabled
  :straight t
  :bind ("<f7>" . org-attach-screenshot)
  :config (setq org-attach-screenshot-dirfunction
                (lambda ()
                  (progn (assert (buffer-file-name))
                         (concat (file-name-sans-extension (buffer-file-name))
                                 "-att")))
                org-attach-screenshot-command-line "gnome-screenshot -a -f %f"))

(use-package deft
  :straight t
  :bind ("<f7>" . deft)
  :custom
  (deft-directory "~/org/roam")
  (deft-recursive t)
  (deft-file-limit 30)
  (deft-current-sort-method 'title))

(provide 'lp-org)

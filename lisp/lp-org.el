(lp-emacs-builtin-package 'org

  ;;; general
  (setq org-directory "~/dropbox/denotes/")
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-startup-folded t)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-cycle-separator-lines 0)
  (setq org-fold-catch-invisible-edits 'show)
  (setq org-return-follows-link t)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-insert-heading-respect-content t) ;; m-ret keeps the content
  (setq org-hide-emphasis-markers nil)

  (setq org-list-allow-alphabetical t)
  (setq org-use-sub-superscripts t)   ; not a big fan of the ambiguity, but trying it

  ;;; TODOs and refiles -- courtesy of some of prot's configuration of
  ;;; course
  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-enforce-todo-dependencies nil)
  (setq org-enforce-todo-checkbox-dependencies nil)

  ;; fontification
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-highlight-latex-and-related '(latex entities)) ; other options affect elisp regexp in src blocks

  ;; (setq org-refile-targets
  ;;       `((,(directory-files org-directory t ".*.org") . (:maxlevel . 2))
  ;;         (nil . (:maxlevel . 2))))
  (setq org-refile-targets '((nil . (:maxlevel . 4))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; (setq org-refile-use-cache t)
  (setq org-reverse-note-order t)

  ;;; tags--should probably be the same as in denote? or file-specific
  (setq org-tag-alist
        '())
  (setq org-agenda-files '("~/dropbox/denotes/--todos__list@@20250729T142757.org"))

  (setq org-auto-align-tags t)
  (setq org-tags-column 80)

  ;;; logs
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (shell . t)
     (julia . t)))

  ;;; archiving
  (setq org-archive-location "::* Archive")
  (setq org-agenda-skip-archived-trees t)
  (setq org-columns-skip-archived-trees t)
  (setq org-cycle-open-archived-trees nil)

  ;;; keybinds
  (define-key text-mode-map (kbd "C-c q") #'auto-fill-mode)
  (define-key global-map (kbd "C-c l") #'org-store-link)

  (let ((map global-map))
    (define-key map (kbd "C-c l") #'org-store-link)
    (define-key map (kbd "C-c o") #'org-open-at-point-global)
    (define-key map (kbd "C-c C-a") #'org-agenda))

  ;; Disable the gorillion keys that org binds
  (let ((map org-mode-map))
    (define-key map (kbd "C-'") nil)
    (define-key map (kbd "C-,") nil)
    (define-key map (kbd "M-;") nil)
    (define-key map (kbd "<C-return>") nil)
    (define-key map (kbd "<C-S-return>") nil)
    (define-key map (kbd "C-M-S-<right>") nil)
    (define-key map (kbd "C-M-S-<left>") nil)
    (define-key map (kbd "C-c C-j") nil) ; org-goto
    (define-key map (kbd "C-c C-o") nil)
    (define-key map (kbd "C-c C-a") nil) ; org-attach
    (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
    (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display)
    (define-key map (kbd "C-c o") #'org-open-at-point))

  (setq org-agenda-diary-file 'diary-file) ; for inserting diary
					; entries from agenda
)


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
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
 
 (setq org-latex-pdf-process '("%latex %b"
			       "bibtex %b"
			       "%latex %b"))
 (setq org-latex-compiler "LaTeX"))
  


(provide 'lp-org)

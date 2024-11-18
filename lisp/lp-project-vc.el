(lp-emacs-builtin-package 'project
  (define-key global-map (kbd "C-x p a") 'ff-find-other-file))

(lp-emacs-git-package 'project-x
  "https://github.com/karthink/project-x.git"
  (setq project-x-window-list-file (locate-user-emacs-file "project-x-window-list"))
  (setq project-x-local-identifier ".project")
  (project-x-mode 1))

;;; TODO: notes on vc use-cases
(lp-emacs-builtin-package 'vc
  (setq vc-handled-backends '(SVN Git))
    ;;;  As always, from Prot. Directly copied. No shame. See https://protesilaos.com/emacs/dotemacs#h:31deeff4-dfae-48d9-a906-1f3272f29bc9

  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest

  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
  \\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
  \\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
  \\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (add-hook 'log-view-mode-hook #'hl-line-mode)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch
    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
    (define-key map (kbd "C-x v o") #'vc-log-outgoing)
    (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
    (define-key map (kbd "C-x v d") #'vc-diff))
  
  (let ((map vc-dir-mode-map))
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming) ; git fetch, replaces `vc-dir-find-file' (use RET)
    (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
    (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
    (define-key map (kbd "k") #'vc-dir-clean-files)
    (define-key map (kbd "G") #'vc-revert)
    (let ((lp-vc-git-branch-map (make-sparse-keymap)))
      (define-key map "b" lp-vc-git-branch-map)
      (define-key lp-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
      (define-key lp-vc-git-branch-map "b" #'vc-retrieve-tag) ; switch branch/tag
      (define-key lp-vc-git-branch-map "l" #'vc-print-branch-log)))
  
  (let ((map vc-git-stash-shared-map))
    (define-key map "a" 'vc-git-stash-apply-at-point)
    (define-key map "c" 'vc-git-stash) ; "create" named stash
    (define-key map "D" 'vc-git-stash-delete-at-point)
    (define-key map "p" 'vc-git-stash-pop-at-point)
    (define-key map "s" 'vc-git-stash-snapshot))
  
  (let ((map vc-annotate-mode-map))
    (define-key map (kbd "C-c C-c") #'vc-annotate-toggle-annotation-visibility)
    (define-key map (kbd "<return>") #'vc-annotate-goto-line)
    (define-key map (kbd "M-<return>") #'vc-annotate-find-revision-at-line))
  
  (let ((map log-view-mode-map))
    (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
    (define-key map (kbd "<return>") #'log-view-find-revision)
    (define-key map (kbd "s") #'vc-log-search)
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming)
    (define-key map (kbd "F") #'vc-update)
    (define-key map (kbd "P") #'vc-push)))

(lp-emacs-elpa-package 'magit
  (setq
   ;; don't put "origin-" in front of new branch names by default
   magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
   magit-push-always-verify t)
  (define-key global-map (kbd "C-x g") 'magit-status))

(provide 'lp-project-vc)

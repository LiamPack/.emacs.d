                                        ; elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(use-package elfeed
  :straight t
  :defer t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq shr-width 80)

  (setq-default elfeed-search-filter "@2-weeks-ago +unread ")

  (defun lp/elfeed-show-all ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-all"))
  (defun lp/elfeed-show-emacs ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-emacs"))
  (defun lp/elfeed-show-daily ()
    (interactive)
    (bookmark-maybe-load-default-file)
    (bookmark-jump "elfeed-daily"))

  ;; Entries older than 2 weeks are marked as readn
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))


  ;; code to add and remove a starred tag to elfeed article
  ;; based on http://matt.hackinghistory.ca/2015/11/22/elfeed/

  ;; add a star
  (defun bjm/elfeed-star ()
    "Apply starred to all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
           (tag (intern "starred")))

      (cl-loop for entry in entries do (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  ;; remove a start
  (defun bjm/elfeed-unstar ()
    "Remove starred tag from all selected entries."
    (interactive )
    (let* ((entries (elfeed-search-selected))
           (tag (intern "starred")))

      (cl-loop for entry in entries do (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  ;; face for starred articles
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "*") 'bjm/elfeed-star))
  (eval-after-load 'elfeed-search
    '(define-key elfeed-search-mode-map (kbd "8") 'bjm/elfeed-unstar)))

(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defalias 'elfeed-toggle-star
;;   (elfeed-expose #'elfeed-search-toggle-all 'star))

;; (eval-after-load 'elfeed-search
;;   '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lp-elfeed)

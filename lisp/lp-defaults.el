;;; Emacs defaults
(lp-emacs-builtin-package 'emacs
  ;; hacky way to remove the =custom-variables= generation in =init.el=
  (setq custom-file (make-temp-file "emacs-custom-"))

  ;; why do these binds exist
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  ;; minimizes GC interference with user activity
  (setq gc-cons-threshold (* 16 1024 1024))

  ;; i fat-finger way too much to not have the confirmation
  (setq confirm-kill-emacs #'yes-or-no-p)
  (global-hl-line-mode t)               ; highlight current line
  (setq fill-column 72)         ; column length

  (setq mode-line-compact t)
  (setq-default mode-line-format
		'("%e"
                  mode-line-front-space
                  (:propertize
		   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
		   display (min-width (5.0)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " -- "
                  mode-line-position
		  (vc-mode vc-mode)
		  " -- "
                  mode-line-modes
                  " -- "
                  mode-line-misc-info
                  mode-line-end-spaces))

  ;; name on top of frame
  (setq frame-title-format '("%b [%m]"))

  (setq warning-minimum-level :error)   ; avoid warning buffer

  ;; scroll
  (setq auto-window-vscroll nil)  ; avoid next-line to trigger line-move-partial
  (setq scroll-conservatively 10)
  (setq scroll-margin 5)
  (setq scroll-step 0)                  ; see info of `scroll-step`: as long as `scroll-conservatively`
                                        ; is a "large value" this should be fine
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'set-horizontal-scroll-bar-mode)
    (set-horizontal-scroll-bar-mode nil))

  (setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
  (setq mouse-wheel-follow-mouse 't)

  ;; other basiscs
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
  (setq create-lockfiles nil)

  ;; answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  (if (display-graphic-p)
      (blink-cursor-mode 1)
    (progn
      (blink-cursor-mode -1)
      (setq visible-cursor t)))

  (setq initial-scratch-message ";; Present Day
"
        visible-bell t)

  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)

  (setq indent-tabs-mode nil                 ; don't insert tab when indent
        help-window-select t                 ; focus on help window when openend
        window-combination-resize nil) ; i'd rather do this myself

  (defun lp--clean-up-buffer-or-region ()
    "Untabifies, indents and deletes trailing whitespace from buffer or region."
    (interactive)
    (save-excursion
      (unless (region-active-p)
        (mark-whole-buffer))
      (untabify (region-beginning) (region-end))
      (indent-region (region-beginning) (region-end))
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (delete-trailing-whitespace))))

  (define-key global-map (kbd "C-x k") #'(lambda () (interactive) (kill-buffer nil)))
  (define-key global-map (kbd "C-x K") #'(lambda () (interactive) (kill-buffer nil) (delete-window)))
  (define-key global-map (kbd "C-c n") #'(lambda () (interactive) (whitespace-cleanup)))
  (define-key global-map (kbd "<f5>")  #'revert-buffer)

  (define-key global-map (kbd "M-z") #'zap-up-to-char) ;; i generally go up to a char non-inclusive
  (define-key global-map (kbd "M-Z") #'zap-to-char)

  (define-key global-map (kbd "C-x C-M-e") #'pp-macroexpand-last-sexp)
  (define-key global-map (kbd "C-h j") #'describe-keymap)
  (define-key global-map (kbd "C-c C-j") #'join-line)
  (define-key global-map (kbd "C-S-p") #'(lambda () (interactive) (previous-line 7)))
  (define-key global-map (kbd "C-S-n") #'(lambda () (interactive) (next-line 7)))
  (define-key global-map (kbd "C-S-w") #'(lambda () (interactive) (duplicate-line) (next-line 1)))
  )

;;; Repeating commands
(lp-emacs-builtin-package 'repeat
  (setq repeat-on-final-keystroke t)
  (setq repeat-exit-timeout 5)
  (setq repeat-exit-key "<escape>")
  (setq repeat-keep-prefix nil)
  (setq repeat-check-key t)
  ;; `C-u C-SPC' will pop mark from the buffer-local mark ring, and
  ;; repeating C-SPC will continue popping mark
  (setq set-mark-command-repeat-pop t)
  (repeat-mode 1))

;;; Time on modeline
(lp-emacs-builtin-package 'time
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("America/New_York" "New York (USA)")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ))
  (setq display-time-default-load-average 0
        display-time-use-mail-icon t
        display-time-24hr-format t
        display-time-day-and-date t)

  (display-time-mode 1))

;;; Man pages and info node
(lp-emacs-builtin-package 'man
  (define-key Man-mode-map (kbd "i") #'Man-goto-section)
  (define-key Man-mode-map (kbd "g") #'Man-update-manpage))


;;; Emacs state saving, server starting, saveplace
;; (lp-emacs-builtin-package 'server
;;   (add-hook 'after-init-hook #'server-start))

(lp-emacs-builtin-package 'desktop
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path `(,user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save ".*")
  (setq desktop-buffers-not-to-save ".*")
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

(lp-emacs-builtin-package 'saveplace
  ;; :diminish
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;; Minibuffer history
(lp-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(register-alist kill-ring))
  (add-hook 'after-init-hook #'savehist-mode))

(lp-emacs-builtin-package 'bookmark
;;;; Built-in bookmarking framework (bookmark.el)
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1)

  ;; thanks prot for the idea to autosave bookmarks :)
  (advice-add 'bookmark-set-internal :after (lambda (&rest _) (funcall 'bookmark-save)))
    
  )

;; thanks prot for showing this package off :) !
(lp-emacs-builtin-package 'tooltip
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips nil
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t)))

  (autoload #'tooltip-mode "tooltip")
  (tooltip-mode 1)
  )

;;; Visualize unwanted whitespace
(lp-emacs-builtin-package 'whitespace
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark))))

;;; Advice around useful functions
(defun lp--provide-mark-line-or-region (&rest args)
  "Force interactive arguments to provide (current line->current
  line + 1) if no active region. Otherwise, provide the original
  parameter specification (mark, point, 'region). `ARGS' discarded"
  (interactive
   (if mark-active
       (list (mark) (point) 'region)
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'lp--provide-mark-line-or-region)
(advice-add 'kill-region :before #'lp--provide-mark-line-or-region)

(define-advice load-theme (:before (&rest args) disable-active-themes)
  "Disable all active themes before loading a new theme."
  (mapc #'disable-theme custom-enabled-themes))

(defun block-undo (fn &rest args)
  "Wrap function `FN' with `ARGS' and allow block-undo of the `FN'
  operation rather than atomized undo for each interactive emacs
  function."
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              dot-mode-execute
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo))


;;; Handle long lines
(lp-emacs-builtin-package 'so-long
  (global-so-long-mode +1))

(provide 'lp-defaults)

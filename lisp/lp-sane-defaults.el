;; set up melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(require 'dash)
(use-package s
  :straight t)

(setf backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message ";; Present Day
"
      wdired-allow-to-change-permissions t
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function 'ignore
      custom-safe-themes t)
;; stop truncating lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
;; also fonts
(set-locale-environment "UTF-8")

;;  GUIs :(
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(display-time-mode t)
(set-face-attribute 'default nil :height 180)
(set-frame-font "Ubuntu Mono" nil t)

;; Too distracting
(blink-cursor-mode -1)
(global-hl-line-mode 1)

;; Don't jerk me around
(electric-indent-mode -1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always use the one true encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively 100
      scroll-preserve-screen-position t)

;; fill-column at 80 is the CS dept standard
(setq fill-column 80)
(setq fci-rule-column 80)
(setq column-number-mode t)

;; fuck cursor lagging on moving
(setq auto-window-vscroll nil)

;; paren mode matching
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Disk space is cheap. Save lots.
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; quicktramp setup
(setq tramp-default-method "ssh")
;; Create own directory for ~ backup file clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)

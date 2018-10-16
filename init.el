;;; package -- my config
;;; Commentary: more of my config
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)

(require 'org)
(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))

;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
;; (package-initialize)

;; ;; chicken before the
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-verbose t)
;; (setq use-package-always-ensure t)


;; ;;; Annoying Defaults
;; (setf backup-inhibited t
;;       auto-save-default nil
;;       auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
;;       inhibit-startup-message t
;;       initial-scratch-message nil
;;       wdired-allow-to-change-permissions t
;;       echo-keystrokes 0.1
;;       delete-active-region nil
;;       disabled-command-function nil
;;       custom-file (make-temp-file "emacs-custom")
;;       large-file-warning-threshold 536870911
;;       gc-cons-threshold (* 1024 1024 32)
;;       ring-bell-function (lambda ()))

;; ;; GUIs are for newbs
;; (menu-bar-mode -1)
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ;; Too distracting
;; (blink-cursor-mode -1)

;; ;; Don't jerk me around
;; (electric-indent-mode -1)

;; ;; I never want to use this
;; (when (fboundp 'set-horizontal-scroll-bar-mode)
;;   (set-horizontal-scroll-bar-mode nil))

;; ;; I hate typing
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; ;; Always use the one true encoding
;; (prefer-coding-system       'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)

;; ;; Insert key is stupid
;; (define-key global-map [(insert)] nil)
;; (define-key global-map [(control insert)] 'overwrite-mode)

;; ;; I hate hitting this by accident
;; (global-set-key (kbd "C-<up>") #'previous-line)
;; (global-set-key (kbd "C-<down>") #'next-line)

;; ;; Magit is the only front-end I care about
;; (setf vc-handled-backends nil
;;       vc-follow-symlinks t)

;; ;; Stop scrolling by huge leaps
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;       scroll-conservatively most-positive-fixnum
;;       scroll-preserve-screen-position t)
;;                                         ;(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))

;; ;; fill-column at 80 is the CS dept standard
;; ;; Fill column + always show column
;; (setq fill-column 80)
;; (setq fci-rule-column 80)
;; (setq column-number-mode t)

;; (setq-default indent-tabs-mode nil)
;; (delete-selection-mode)
;; (global-set-key (kbd "RET") 'newline-and-indent)


;; (setf backup-inhibited t
;;       auto-save-default nil
;;       auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
;;       inhibit-startup-message t
;;       initial-scratch-message nil
;;       wdired-allow-to-change-permissions t
;;       echo-keystrokes 0.1
;;       delete-active-region nil
;;       disabled-command-function nil
;;       custom-file (make-temp-file "emacs-custom")
;;       large-file-warning-threshold 536870911
;;       gc-cons-threshold (* 1024 1024 32)
;;       ring-bell-function (lambda ()))





(provide 'init)
;;; init.el ends here

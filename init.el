(add-to-list 'load-path "~/.emacs.d/lisp/personal-packages/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/personal-packages/")
;; Do not initialise installed packages (I use `straight.el')
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)
(setq use-dialog-box t)               ; only for mouse events
(setq use-file-dialog nil)

(setq straight-use-package-by-default nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

;; provides `straight-x-clean-unused-repos' (part of `straight.el')
(use-package straight-x)

(use-package vc
  :config
  (setq vc-follow-symlinks t)) ; Because my dotfiles are managed that way
(use-package diminish
  :straight t)
(use-package server
  :hook (after-init-hook . server-start))
(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; Mark safe variables early so that tangling won't break
(put 'after-save-hook 'safe-local-variable
     (lambda (value) (equal value '(org-babel-tangle t))))
(put 'display-line-numbers-width 'safe-local-variable 'integerp)

;; Tangle and compile if necessary only, then load the configuration
(let* ((.org (expand-file-name "config.org" user-emacs-directory))
       (.el (concat (file-name-sans-extension .org) ".el"))
       (modification-time
        (file-attribute-modification-time (file-attributes .org))))
  (require 'org-macs)
  (unless (org-file-newer-than-p .el modification-time)
    (require 'ob-tangle)
    (org-babel-tangle-file .org .el "emacs-lisp"))
  (load-file .el))

;; Collect garbage when all else is done
(garbage-collect)

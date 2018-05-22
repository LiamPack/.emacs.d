;;; init.el --- -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/etc")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(setq custom-safe-themes t)
(setq package-enable-at-startup nil
      ;; work around package.el bug in Emacs 25
      package--init-file-ensured t)
(package-initialize)
;; some things to not forget about when using emacs!  For those who
;; don’t know: C-u 1 C-y is equivalent to plain C-y, but C-u 2 C-y (or
;; just C-2 C-y) inserts the previous killed text (much like C-y M-y),
;; and also marks it as the current one. With higher arguments, it
;; inserts earlier kills.

                                        ; general setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;; chicken before the
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)


(require 'unannoy)
(require 'extras)
(require 'utility)


;;; Time to load everything from the lisp/ directory
(setq active-directory-files (list "~/.emacs.d/lisp/"))
(defun active-config-directory ()
  "Where active package configurations are kept."
  (format "%slisp/" user-emacs-directory))

(defun load-use-file (name)
  "Load a use file NAME expect an error if it doesn't map to an existing file."
  (let (file)
    (setq file (concat (active-config-directory) name))
    (unless (or (equal name ".") (equal name ".."))
      (message "Using config: %s" file)
      (if (file-exists-p file)
          (load-file file)
        (message "Warning: %s doesn't exist" file)))))

(dolist (use-file
         (directory-files (active-config-directory)))
  (load-use-file use-file))

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-x p") 'pop-to-mark-command)
;;(global-set-key (kbd "C-?") 'help-command)


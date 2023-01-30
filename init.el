(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)

;; automatic package installation
(setq package-enable-at-startup t)
;; (setq package-quickstart t)

(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)
(setq use-dialog-box t)               ; only for mouse events
(setq use-file-dialog nil)

(require 'comp)
(setq native-comp-async-report-warnings-errors 'silent)

(defvar lp-emacs-ensure-builtin-missed '()
  "A set of built-in packages which failed a `require' call.")

(defmacro lp-emacs-builtin-package (package &rest body)
  (declare (indent 1))
  `(progn
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (print (format "[Warning]: Loading `%s' failed" ,package))
       (display-warning 'lp-emacs (format "Loading `%s' failed" ,package) :warning)
       (display-warning
        'lp-emacs
        "See `lp-emacs-ensure-builtin-missed' for a set of missed packages that failed install"
        :warning)
       (add-to-list 'lp-emacs-ensure-builtin-missed ,package))))


(defvar lp-emacs-ensure-install-missed '()
  "A set of packages which failed a `package-install' call.")

(defmacro lp-emacs-elpa-package (package &rest body)
  (declare (indent 1))
  `(progn
     (condition-case nil
	 (when (not (package-installed-p ,package))
	   (package-install ,package))
       nil)
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (print (format "[Warning]: Loading `%s' failed" ,package))
       (display-warning 'lp-emacs (format "Loading `%s' failed" ,package) :warning)
       (display-warning
        'lp-emacs
        "See `lp-emacs-ensure-installed-missed' for a set of missed packages that failed install"
        :warning)
       (add-to-list 'lp-emacs-ensure-install-missed ,package))))

(defmacro lp-emacs-git-package (package repo-name &rest body)
  (declare (indent 1))
  `(progn
     (let ((local-dir "~/.emacs.d/local"))
       (unless (file-directory-p (file-name-concat local-dir (file-name-base ,repo-name)))
         (shell-command
          (format "mkdir -p %s && cd %s && git clone %s"
                  local-dir local-dir ,repo-name)))
       (add-to-list 'load-path
                    (file-name-concat
                     local-dir (file-name-base ,repo-name))))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (print (format "[Warning]: Loading `%s' failed" ,package))
       (display-warning 'lp-emacs (format "Loading `%s' failed" ,package) :warning)
       (display-warning
        'lp-emacs
        "See `lp-emacs-ensure-installed-missed' for a set of missed packages that failed install"
        :warning)
       (add-to-list 'lp-emacs-ensure-install-missed ,package))))

(lp-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables '("PATH" "JAVA_HOME"))
  (setenv "JAVA_HOME" "/usr/lib/jvm/java-1.11.0-openjdk-amd64/")
  (exec-path-from-shell-initialize))

(defvar lp--lisp-packages
  '(lp-defaults
    lp-aesthetics
    lp-calendar
    lp-completion
    lp-editing
    lp-external
    lp-minibuffer
    lp-org
    lp-pdf
    lp-programming
    lp-project-vc
    lp-tex
    lp-time
    lp-unix
    lp-window
    lp-writing
    lp-denote
    ;; lp-experimental
    ))

(dolist (p lp--lisp-packages)
  (require p))

(load-file (expand-file-name "post-init.el" user-emacs-directory))

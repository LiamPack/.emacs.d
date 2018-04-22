;;; init.el --- -*- lexical-binding: t; -*-
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
(package-initialize)

;; some things to not forget about when using emacs!  For those who
;; don’t know: C-u 1 C-y is equivalent to plain C-y, but C-u 2 C-y (or
;; just C-2 C-y) inserts the previous killed text (much like C-y M-y),
;; and also marks it as the current one. With higher arguments, it
;; inserts earlier kills.

                                        ; general setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(setq use-package-always-ensure t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;; Create own directory for ~ backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Disk space is cheap. Save lots.
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; we don't live in the past. bump that memory up.
(setq gc-cons-threshold 50000000)

;; fuck cursor lagging on moving
(setq auto-window-vscroll nil)

(setq fill-column 80)

;; paren mode
(setq show-paren-mode t)

;; Disable window chrome
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)
                                        ;      initial-scratch-message (format ";; %s\n" (adafruit-wisdom-select)))

;; stop truncating lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)
;; * good themes
;; ** base16
;;    * zenburn
;;    * unikitty light
;;    * solarized light
;;    * rebecca
;;    * porple
;;    * phd
;;    * ocean
;;    * nord
;;    * monokai
;;    * mocha
;;    * mellow-purple
;;    * material + material palenight
;;    * harmonic-{light,dark}
;;    * cupertino
;;    * cupcake
;;    * sulphurpool-light
;;    * heath-light
;;    * cave-light
;; * avk-daylight

;; powerline is terrific
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; also fonts
(set-locale-environment "UTF-8")

;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (powerline-buffer-size face0 'l)
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left face0 face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (center (list (powerline-raw global-mode-string face1 'r)
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 face0)
                                          (powerline-raw " ")
                                          (powerline-raw "%6p" face0 'r)
                                          (powerline-hud face2 face1)
                                          ))
                            (rhs (list (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face2 'l))
                                       (powerline-major-mode face2 'l)
                                       (powerline-process face2)
                                       (powerline-raw " :" face2)
                                       (powerline-minor-modes face2 'l)
                                       (powerline-raw " " face2)
                                       (funcall separator-right face2 face1)
                                       ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-right-theme))



(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(use-package moe-theme
  :ensure t
  :disabled
  :config
  (setq moe-light-pure-white-background-in-terminal t)
  (setq show-paren-style 'expression)
  (moe-theme-set-color 'purple)
  ;; Resize titles
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
  (require 'moe-theme-switcher)
                                        ;(setq moe-theme-highlight-buffer-id t)
  )




                                        ; themes!?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; espresso ; cyberpunk ; moe-light ;

;; wrap visual lines! it helps.
(global-visual-line-mode 1)


;; im sick of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fancy lambdas
(global-prettify-symbols-mode t)

;; screw the bell
(setq ring-bell-function 'ignore)

;; Scroll conservatively
(setq scroll-conservatively 100)

;; time on modeline is cool
(use-package time
  :ensure t
  :config
  (setf display-time-default-load-average nil
        display-time-use-mail-icon t
        display-time-24hr-format t)
  (display-time-mode t))

;; Font functionality
                                        ; iosevka, consolas, source code pro, Fira Code, dejavu
(setq lp/default-font "DejaVu Sans Mono")
(setq lp/default-font-size 12)

(setq lp/current-font-size lp/default-font-size)

;; Define the factor that we should go by when increasing/decreasing
(setq lp/font-change-increment 1.1)

(defun lp/set-font-size ()
  "Set the font to 'lp/default-font' at 'lpcurrent-font-size'."
  (set-frame-font
   (concat lp/default-font "-" (number-to-string lp/current-font-size))))

(defun lp/reset-font-size ()
  "Change font back to default size"
  (interactive)
  (setq lp/current-font-size lp/default-font-size)
  (lp/set-font-size))

(defun lp/increase-font-size ()
  "increase current font size by a factor of 'lp/font-change-increment'."
  (interactive)
  (setq lp/current-font-size
        (ceiling (* lp/current-font-size lp/font-change-increment)))
  (lp/set-font-size))

(defun lp/decrease-font-size ()
  (interactive)
  (setq lp/current-font-size
        (floor (/ lp/current-font-size lp/font-change-increment)))
  (lp/set-font-size))

(define-key global-map (kbd "C-0") 'lp/reset-font-size)
(define-key global-map (kbd "C-=") 'lp/increase-font-size)
(define-key global-map (kbd "C--") 'lp/decrease-font-size)

(lp/reset-font-size)

;; (mapc
;;  (lambda (face)
;;    (set-face-attribute
;;     face
;;     'nil
;;     :family "Fira Code"
;;     :height 130
;;     :width 'normal
;;     :weight 'normal))
;;  '(default))
                                        ; (set-face-attribute
                                        ;  'variable-pitch
                                        ;  'nil
                                        ;  :family "Helvetica Neue"
                                        ;  :height 150)
;;  TODO: Tlinum mode with spaces?
;; (global-linum-mode 1)
;; (defadvice linum-update-window (around linum-dynamic activate)
;;   (let* ((w (length (number-to-string
;;                      (count-lines (point-min) (point-max)))))
;;          (linum-format (concat " %" (number-to-string w) "d ")))
;;     ad-do-it))
;;(setq fci-rule-color (face-attribute 'linum :foreground))

;; Fill column + always show column
(setq fci-rule-column 80)
(setq column-number-mode t)

;; global-hl-line-mode softly highlights bg color of line. Its nice.
(when window-system
  (global-hl-line-mode))

;; Helps with stupid ^L characters - allows a page break to appear!
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))



                                        ; useful functions + keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14 ")

(defun lp/kill-current-buffer ()
  "Just kill the buffer man (no prompt when killing buffer)"
  (interactive)
  (kill-buffer (current-buffer)))

(defun lp/generate-scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun lp/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on whitespace content. Does
  not indent buffer!"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(defun lp/cleanup-buffer ()
  "Perform bunch of operations on the whitespace content of buffer."
  (interactive)
  (lp/cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))


;; Always killcurrent buffer
(global-set-key (kbd "C-x k") 'lp/kill-current-buffer)

;; Look for executables in bin
(setq exec-path (append exec-path '("/user/local/bin")))

;; Always use spaces for indentation
;; fuck tabs
(setq-default indent-tabs-mode nil)

;; Lets bind C-c C-k to compile buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; hippie expand is quite nice for aut-completing
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-TAB") 'hippie-expand)
                                        ;(global-set-key (kbd "M-o") 'other-window)

;; Gotta keep those buffers clean
(global-set-key (kbd "C-c n") 'lp/cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'lp/cleanup-buffer)

;; Open up a randomly generated scratch buffer (cause i like scratch buffers)
(global-set-key (kbd "<f12>") 'lp/generate-scratch-buffer)

;; Compile on a keybind
(global-set-key (kbd "C-<f7>") 'compile)

;; Quickly comment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; fuk these defaults
(global-set-key (kbd "M-g") #'goto-line)

(global-set-key (kbd "C-@") #'align-regexp)

;; backspace change!
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-m") 'newline-and-indent)


;; pop to the last command mark! its cool.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode t)
  (global-set-key (kbd "C-x <deletechar>") 'global-hungry-delete-mode))

(use-package ediff
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-diff-options "-w")
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (derived-mode-p 'outline-mode)
                (outline-show-all)))))


                                        ; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management - Balance, switching, and splitting

;; I almost always want to switch to a window when I split. So lets do that.

(defun lp/split-window-below-and-switch ()
  "Split window horizontally, then switch to that new window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun lp/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'lp/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'lp/split-window-right-and-switch)


;; ace-window stuff

;; You can also start by calling ace-window and then decide to switch the action to delete or swap etc. By default the bindings are:

;;     x - delete window
;;     m - swap windows
;;     M - move window
;;     j - select buffer
;;     n - select the previous window
;;     u - select buffer in the other window
;;     c - split window fairly, either vertically or horizontally
;;     v - split window vertically
;;     b - split window horizontally
;;     o - maximize current window
;;     ? - show these command bindings

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq  aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))



;;; Hide a whole bunch of stuff on the modeline. It's a bit annoying.
;;; Using the =diminish= package for this.
(use-package diminish
  :ensure t
  :config

  (defmacro diminish-minor-mode (filename mode &optional abbrev)
    `(eval-after-load (symbol-name ,filename)
       '(diminish ,mode ,abbrev)))

  (defmacro diminish-major-mode (mode-hook abbrev)
    `(add-hook ,mode-hook
               (lambda () (setq mode-name ,abbrev))))

  (diminish-minor-mode 'abbrev 'abbrev-mode)
  (diminish-minor-mode 'simple 'auto-fill-function)
  (diminish-minor-mode 'company 'company-mode)
  (diminish-minor-mode 'eldoc 'eldoc-mode)
  (diminish-minor-mode 'flycheck 'flycheck-mode)
  (diminish-minor-mode 'flyspell 'flyspell-mode)
  (diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
  (diminish-minor-mode 'projectile 'projectile-mode)
  (diminish-minor-mode 'ruby-end 'ruby-end-mode)
  (diminish-minor-mode 'subword 'subword-mode)
  (diminish-minor-mode 'undo-tree 'undo-tree-mode)
  (diminish-minor-mode 'yard-mode 'yard-mode)
  (diminish-minor-mode 'yasnippet 'yas-minor-mode)
  (diminish-minor-mode 'wrap-region 'wrap-region-mode)
  (diminish-minor-mode 'simple 'visual-line-mode)
  (diminish-minor-mode 'paredit 'paredit-mode " π")
  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'haskell-mode-hook "λ=")
  (diminish-major-mode 'lisp-interaction-mode-hook "λ")
  (diminish-major-mode 'python-mode-hook "Py"))



;; calc is here for some reason
(use-package calc
  :ensure t
  :bind ("C-c =" . calc))


                                        ; programming environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mark TODOs as red n stuff
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face prepend)))))

;;; Magit
;; God bless magit and all that it does
(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t)

  :bind ("C-x g" . magit-status))

;;; Dired
;; clean up permissions and owners, less noisy
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))
;; disable ls by default
(setq dired-use-ls-dired nil)

(use-package projectile
  :ensure t
  :config
  (require 'projectile)

  ;; Projectile everywhere obviously
  (projectile-global-mode)

  (defun lp/search-project-for-symbol-at-point ()
    "Use projectile-ag to search current project for the current symbol."
    (interactive)
    (projectile-ag (projectile-symbol-at-point)))
  (global-set-key  (kbd "C-c v") 'projectile-ag)
  (global-set-key (kbd "C-c C-v") 'lp/search-project-for-symbol-at-point))





                                        ; eshell my goodness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chec kout
;; [[https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org][this
;; guy]] for some of his amazing eshell config

;;; Currently using eshell for my shell sessions. Bound to C-c s.
;; Going to do some setup for this though
(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))
(use-package eshell
  :ensure t
  :init
  (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
   eshell-scroll-to-bottom-on-input 'all
   eshell-error-if-no-glob t
   eshell-hist-ignoredups t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions nil
   eshell-destroy-buffer-when-process-dies t))


(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")

                              (eshell/alias "gd" "magit-diff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "d" "dired $1")

                              ;; The 'ls' executable requires the Gnu version on the Mac
                              (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                            "/usr/local/bin/gls"
                                          "/bin/ls")))
                                (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

(global-set-key (kbd "C-c s") 'eshell)

;; Change up some
(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output))
           (git-icon  "\xe0a0")
           (git-icon2 (propertize "\xf020" 'face `(:family "octicons"))))
      (concat git-repo " " git-icon2 " " git-branch))))

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(defun ruby-prompt ()
  "Returns a string (may be empty) based on the current Ruby Virtual Environment."
  (let* ((executable "~/.rvm/bin/rvm-prompt")
         (command    (concat executable "v g")))
    (when (file-exists-p executable)
      (let* ((results (shell-command-to-string executable))
             (cleaned (string-trim results))
             (gem     (propertize "\xe92b" 'face `(:family "alltheicons"))))
        (when (and cleaned (not (equal cleaned "")))
          (s-replace "ruby-" gem cleaned))))))

(defun python-prompt ()
  "Returns a string (may be empty) based on the current Python
   Virtual Environment. Assuming the M-x command: `pyenv-mode-set'
   has been called."
  (when (fboundp #'pyenv-mode-version)
    (let ((venv (pyenv-mode-version)))
      (when venv
        (concat
         (propertize "\xe928" 'face `(:family "alltheicons"))
         (pyenv-mode-version))))))

(defun eshell/eshell-local-prompt-function ()
  "A prompt for eshell that works locally (in that is assumes
that it could run certain commands) in order to make a prettier,
more-helpful local prompt."
  (interactive)
  (let* ((pwd        (eshell/pwd))
         (directory (split-directory-prompt
                     (pwd-shorten-dirs
                      (pwd-replace-home pwd))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (curr-dir-git-branch-string pwd))
         (ruby   (when (not (file-remote-p pwd)) (ruby-prompt)))
         (python (when (not (file-remote-p pwd)) (python-prompt)))

         (dark-env (eq 'dark (frame-parameter nil 'background-mode)))
         (for-bars                 `(:weight bold))
         (for-parent  (if dark-env `(:foreground "dark orange") `(:foreground "blue")))
         (for-dir     (if dark-env `(:foreground "orange" :weight bold)
                        `(:foreground "blue" :weight bold)))
         (for-git                  `(:foreground "green"))
         (for-ruby                 `(:foreground "red"))
         (for-python               `(:foreground "#5555FF")))

    (concat
     (propertize "⟣─ "    'face for-bars)
     (propertize parent   'face for-parent)
     (propertize name     'face for-dir)
     (when branch
       (concat (propertize " ── "    'face for-bars)
               (propertize branch   'face for-git)))
     (when ruby
       (concat (propertize " ── " 'face for-bars)
               (propertize ruby   'face for-ruby)))
     (when python
       (concat (propertize " ── " 'face for-bars)
               (propertize python 'face for-python)))
     (propertize "\n"     'face for-bars)
     (propertize (if (= (user-uid) 0) " #" " $") 'face `(:weight ultra-bold))
     ;; (propertize " └→" 'face (if (= (user-uid) 0) `(:weight ultra-bold :foreground "red") `(:weight ultra-bold)))
     (propertize " "    'face `(:weight bold)))))

(setq eshell-highlight-prompt nil)
(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

;; ehsell here!
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (let* ((height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

(use-package eshell
  :config
  (defun ha/eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much) ; Why not? (eshell/exit)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ("C-d" . ha/eshell-quit-or-delete-char)))))

                                        ; c-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A quick hook to C modes to quickswap to =.h= files or =.c= files. It's nice
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
;; also gdb is cool
(setq gdb-many-windows 't)


(use-package company
  :ensure t
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time

;; from https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
(use-package clang-format
  :defer t
  :ensure t
  :bind (("C-c C-f" . clang-format-region)))


(use-package cc-mode
  :defer t
  :ensure t
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
  (use-package google-c-style
    :ensure t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    (add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode)))
  )

;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; handle very large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

                                        ; slime and lisps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I'd prefer that compilation output goes to *compilation buffer*
;;; Rarely have the window selected, so the output disappears past the
;;; bottom of the window
(setq compilation-scroll-output t)
(setq compilation-window-height 15)


;; quicktramp setup
(setq tramp-default-method "ssh")

;; paren stuff
;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :ensure t)
(use-package rainbow-delimiters
  :ensure t)


;; We want all lispy languages to use =paredit-mode= and =rainbow-delimiters
(setq lisp-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook)) ; can add more or whatever


(setq show-paren-style 'expression)

(dolist (hook lisp-mode-hooks)
  (add-hook hook (lambda ()
                   (paredit-mode)
                   (rainbow-delimiters-mode))))
                                        ; (add-hook 'text-mode-hook 'hook-function)

(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-repl))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; eldoc provides minibuffer hints for elisp things. it's super nice
(use-package "eldoc"
  :ensure t
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package elpy
  :ensure t
  :defer t)

(use-package python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (elpy-enable)
  (setq python-indent-offsett 2))

(use-package company-jedi
  :ensure t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;; flycheck mode is not too bad.
(use-package flycheck
  :ensure t
  :defer t
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )


;; configuration
(use-package yasnippet
  :ensure t
  :functions yas-global-mode yas-expand
  :defer 5
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-triggers-in-field t)
  (setq yas-verbosity 0)
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets/" "~/.emacs.d/elpa/yasnippet-20170923.1646/snippets/"))
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
;; (use-package company-yasnippet
;;   :ensure t
;;   :bind ("C-M-y" . company-yasnippet)
;;   :after (yasnippet))


;; auto yas is pretty damn cool
(use-package auto-yasnippet
  :ensure t
  :bind ((  "C-1" . aya-create)
         (  "C-2" . aya-expand)))


(use-package recentf
  :ensure t
  :diminish recentf-mode
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))


;; Ido configuration
(use-package flx-ido
  :disabled
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (ido-ubiquitous-mode 1)
  (flx-ido-mode 1) ; better/faster matching
  (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :ensure t)

;; ivy stuff
(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))

  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d "))

;; sick of this blinking
(blink-cursor-mode -1)


(use-package counsel
  :ensure t
  :bind (("C-x f" . counsel-find-file)
         ("C-? f" . counsel-describe-function)
         ("C-? v" . counsel-describe-variable)
         ("M-x" . counsel-M-x)
         ("C-c o" . counsel-recentf)
         ("C-x l" . counsel-locate))
  :config
  (setq counsel-find-file-at-point t))

(use-package swiper
  :ensure t
  :defer t
  ;; :bind (:map isearch-mode-map
  ;;             ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind (("C-c C-r" . swiper)
         ("C-c C-s" . counsel-grep-or-swiper)))



;; anzu does active showing of all
(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)

  (custom-set-variables
   '(anzu-mode-lighter "")
   '(nvm-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => "))

  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

                                        ; mc tips and tricks!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     To get out of multiple-cursors-mode, press <return> or C-g. The
;;     latter will first disable multiple regions before disabling
;;     multiple cursors. If you want to insert a newline in
;;     multiple-cursors-mode, use C-j.

;;     (define-key mc/keymap (kbd "<return>") nil) will make <return>
;;     insert a newline; multiple-cursors-mode can still be disabled
;;     with C-g.

;;     Sometimes you end up with cursors outside of your view. You can
;;     scroll the screen to center on each cursor with C-v and M-v or
;;     you can press C-' to hide all lines without a cursor, press C-'
;;     again to unhide.

;;     Try pressing mc/mark-next-like-this with no region selected. It
;;     will just add a cursor on the next line.

;;     Try pressing mc/mark-next-like-this-word or
;;     mc/mark-next-like-this-symbol with no region selected. It will
;;     mark the word or symbol and add a cursor at the next occurance

;;     Try pressing mc/mark-all-like-this-dwim on a tagname in
;;     html-mode.

;;     Notice that the number of cursors active can be seen in the
;;     modeline.

;;     If you get out of multiple-cursors-mode and yank - it will yank
;;     only from the kill-ring of main cursor. To yank from the
;;     kill-rings of every cursor use yank-rectangle, normally found
;;     at C-x r y.

;;     You can use mc/reverse-regions with nothing selected and just
;;     one cursor. It will then flip the sexp at point and the one
;;     below it.

;;     When you use mc/edit-lines, you can give it a positive or
;;     negative prefix to change how it behaves on too short lines.

;;     If you would like to keep the global bindings clean, and get
;;     custom keybindings when the region is active, you can try
;;     region-bindings-mode.

;; BTW, I highly recommend adding mc/mark-next-like-this to a key
;; binding that's right next to the key for er/expand-region.
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("[" . mc/cycle-backward)
              ("]" . mc/cycle-forward)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines))
  :init
  (setq mc/list-file (locate-user-emacs-file "mc-lists"))

  ;; Disable the annoying sluggish matching paren blinks for all cursors
  ;; when you happen to type a ")" or "}" at all cursor locations.
  (defvar modi/mc-blink-matching-paren--store nil
    "Internal variable used to restore the value of `blink-matching-paren'
after `multiple-cursors-mode' is quit.")

  ;; The `multiple-cursors-mode-enabled-hook' and
  ;; `multiple-cursors-mode-disabled-hook' are run in the
  ;; `multiple-cursors-mode' minor mode definition, but they are not declared
  ;; (not `defvar'd). So do that first before using `add-hook'.
  (defvar multiple-cursors-mode-enabled-hook nil
    "Hook that is run after `multiple-cursors-mode' is enabled.")
  (defvar multiple-cursors-mode-disabled-hook nil
    "Hook that is run after `multiple-cursors-mode' is disabled.")

  (defun modi/mc-when-enabled ()
    "Function to be added to `multiple-cursors-mode-enabled-hook'."
    (setq modi/mc-blink-matching-paren--store blink-matching-paren)
    (setq blink-matching-paren nil))

  (defun modi/mc-when-disabled ()
    "Function to be added to `multiple-cursors-mode-disabled-hook'."
    (setq blink-matching-paren modi/mc-blink-matching-paren--store))

  (add-hook 'multiple-cursors-mode-enabled-hook #'modi/mc-when-enabled)
  (add-hook 'multiple-cursors-mode-disabled-hook #'modi/mc-when-disabled))

(use-package expand-region
  :ensure t
  :bind ("C-," . er/expand-region))

                                        ; web dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (define-key web-mode-map (kbd "<backtab>") 'web-mode-fold-or-unfold)

  )



;; hceck it out https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t
  :defer t
  :diminish emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (setq emmet-move-cursor-between-quotes t))


                                        ; org-mode
                                        ; TODO speed-keys?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :ensure t
  :config
  (setq org-ellipsis "⤵"))

(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
         ("\C-cl" . org-store-link)
         ("\C-cb" . org-iswitchb))
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t)))

  ;; use enter to follow links instead of C-c C-o
  (setq org-return-follows-link t)

  ;; NOTE: If this isn't working, make sure to delete /
  ;; byte-recompile the /elpa/org/.. directory!
  ;; enable language compiles
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (shell . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-window-setup 'current-window)


;;;;;;;; file directory setup
  ;; Org-capture management + Tasks
  (setq org-directory "~/Dropbox/org/")

  (defun org-file-path (filename)
    "Return absolute address of an org file give its relative name."
    (concat (file-name-as-directory org-directory) filename))

  (setq org-inbox-file "~/Dropbox/inbox.org")
  (setq org-index-file (org-file-path "index.org"))
  (setq org-personal-file (org-file-path "personal.org"))
  (setq org-school-file (org-file-path "school.org"))
  (setq org-projects-file (org-file-path "projects.org"))
  (setq org-notes-file (org-file-path "notes.org"))
  (setq org-journal-file (org-file-path "journal.org"))
  (setq org-monthly-file (org-file-path "monthly.org"))
  (setq org-archive-location
        (concat (org-file-path "archive.org") "::* From %s"))





  ;; I keep all of my todos in =~/Dropbox/org/index.org= so I derive my
  ;; agenda from there
  (setq org-agenda-files
        (list org-index-file org-personal-file org-school-file org-projects-file org-notes-file org-journal-file))
  (setq all-org-files
        (list org-index-file org-personal-file org-school-file org-projects-file org-notes-file org-journal-file (org-file-path "to-read.org")))

  ;; refiling!
  ;; refiling
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
                ("NEXT" :foreground "blue" :weight bold)
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


  ;; TODO - make a macro for this. it's pretty shitty like this lol. learn how to use macros

  (defun lp/refile-to (file headline)
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

  (defun lp/refile-school ()
    (interactive)
    (while (not (equal nil (search-forward ":school:" nil t)))
      (beginning-of-visual-line)
      (lp/refile-to "~/Dropbox/org/school.org" "inbox"))
    (switch-to-buffer "index.org"))

  (defun lp/refile-personal ()
    (interactive)
    (while (not (equal nil (search-forward ":personal:" nil t)))
      (beginning-of-visual-line)
      (lp/refile-to "~/Dropbox/org/personal.org" "general"))
    (switch-to-buffer "index.org"))

  (defun lp/refile-all ()
    (interactive)
    (beginning-of-buffer)
    (lp/refile-school)
    (beginning-of-buffer)
    (lp/refile-personal)
    (universal-argument)
    (save-some-buffers))

  (defun lp/refile-projects ()
    (interactive)
    (while (not (equal (re-search-forward "\[\#[1-3]\]" nil t) nil))
      (beginning-of-visual-line)
      (lp/refile-to "~/Dropbox/org/projects.org" "projects"))
    (switch-to-buffer "ideas.org"))



  ;;   (setq-default org-preview-latex-default-process 'dvisvgm
  ;;                 org-latex-packages-alist '(("" "tikz" t)
  ;;                                            ("american,siunitx,smartlabels" "circuitikz" t)
  ;;                                            ("" "mathtools" t))
  ;;                 org-latex-preview-ltxpng-directory (locate-user-emacs-file "Latex Previews/")
  ;;                 org-format-latex-options
  ;;                 '(:foreground default :background default :scale 1.7
  ;;                               :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
  ;;                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
  ;;                 org-preview-latex-process-alist
  ;;                 '((dvisvgm :programs ("latex" "dvisvgm")
  ;;                            :description "dvi > svg"
  ;;                            :message "you need to install the programs: latex and dvisvgm."
  ;;                            :use-xcolor t
  ;;                            :image-input-type "dvi"
  ;;                            :image-output-type "svg"
  ;;                            :image-size-adjust (1.7 . 1.5)
  ;;                            :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
  ;;                            :image-converter ("dvisvgm %f -n -b 1 -c %S -o %O"))
  ;;                   (imagemagick :programs ("latex" "convert")
  ;;                                :description "pdf > png"
  ;;                                :message "you need to install the programs: latex and imagemagick."
  ;;                                :use-xcolor t
  ;;                                :image-input-type "pdf"
  ;;                                :image-output-type "png"
  ;;                                :image-size-adjust (1.0 . 1.0)
  ;;                                :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
  ;;                                :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
  ;;                   (dvipng :programs ("latex" "dvipng")
  ;;                           :description "dvi > png"
  ;;                           :message "you need to install the programs: latex and dvipng."
  ;;                           :image-input-type "dvi"
  ;;                           :image-output-type "png"
  ;;                           :image-size-adjust (1.0 . 1.0)
  ;;                           :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
  ;;                           :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f")))
  ;;                 org-format-latex-header
  ;;                 "\\documentclass{article}
  ;; \\usepackage[usenames]{color}
  ;; [PACKAGES]
  ;; [DEFAULT-PACKAGES]
  ;; \\pagestyle{empty}
  ;; \\setlength{\\textwidth}{\\paperwidth}
  ;; \\addtolength{\\textwidth}{-3cm}
  ;; \\setlength{\\oddsidemargin}{1.5cm}
  ;; \\addtolength{\\oddsidemargin}{-2.54cm}
  ;; \\setlength{\\evensidemargin}{\\oddsidemargin}
  ;; \\setlength{\\textheight}{\\paperheight}
  ;; \\addtolength{\\textheight}{-\\headheight}
  ;; \\addtolength{\\textheight}{-\\headsep}
  ;; \\addtolength{\\textheight}{-\\footskip}
  ;; \\addtolength{\\textheight}{-3cm}
  ;; \\setlength{\\topmargin}{1.5cm}
  ;; \\addtolength{\\topmargin}{-2.54cm}
  ;; \\tikzset{every picture/.style={color=fg}}")

  ;; NOTE(nox): Get different latex fragments for different themes
                                        ; agenda stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-agenda-tags-column 80)
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t) ;; nil為加上分隔線，t為去掉
  ;; 用describe-char來查你想要的seperator char code
  (setq org-agenda-block-separator 45)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NOX'S SHIT
  (defun nox/org-agenda-finalize ()
    ;; NOTE(nox): Reset project hierarchy builder helper variable
    (setq nox/org-agenda-first-project t)

    ;; NOTE(nox): Remove empty blocks
    (save-excursion
      (goto-char (point-min))
      (let ((prev (if (get-text-property (point-min) 'org-agenda-structural-header)
                      (point-min)
                    (next-single-property-change (point-min) 'org-agenda-structural-header)))
            next)
        (while (and prev (/= prev (point-max)))
          (setq next
                (or (next-single-property-change (next-single-property-change prev 'org-agenda-structural-header)
                                                 'org-agenda-structural-header)
                    (point-max)))
          (if (or (and (< next (point-max)) (< (count-lines prev next) 4))
                  (and (= next (point-max)) (< (count-lines prev next) 2)))
              (delete-region prev next)
            (setq prev next)))))

    ;; NOTE(nox): Turn root projects bold
    (save-excursion
      (while (search-forward (char-to-string ?\u200B) nil t)
        (add-face-text-property (line-beginning-position) (1+ (line-end-position)) '(:weight bold)))))

  (setq-default
   org-agenda-custom-commands
   '(("n" "Agenda"
      ((agenda ""
               ((org-agenda-files (list org-index-file org-personal-file org-school-file org-projects-file org-notes-file org-journal-file org-monthly-file))
                (org-agenda-skip-scheduled-if-deadline-is-shown t)))
       (tags "cs87|cs73"
             ((org-agenda-overriding-header "CS Work")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags "jpns"
             ((org-agenda-overriding-header "JPNS")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags "kizuna|smash|outsiders"
             ((org-agenda-overriding-header "Clubs")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header "Personal Stuff")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-files (list org-personal-file))))
       )
      )
     ("t" "To Read Stuff"
      ((tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header "Stuff to Read")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org"))))))))

   org-agenda-span 'week
   org-agenda-prefix-format '((agenda . "  %?-12t% s")
                              (todo   . "  ")
                              (tags   . "  ")
                              (search . "  "))
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
   org-agenda-dim-blocked-tasks nil
   org-agenda-block-separator ""
   org-agenda-time-grid '((daily today require-timed) nil "......" "----------------"))
  ;; Custom agenda command definitions
                                        ; ((org-agenda-finalize-hook 'nox/org-agenda-finalize))
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
  (setq org-log-done 'time)             ; also record when the TODO was archived

  (setq org-capture-templates
        '(("g" "Groceries"
           entry
           (file "~/Dropbox/org/groceries.org")
           "- [ ]n %?\n")
          ("i" "Ideas"
           entry
           (file+headline "~/Dropbox/org/ideas.org" "Project Ideas")
           "** [#%^{9}] %?\n")
          ("j" "Journal"
           entry
           (file+datetree "~/Dropbox/org/journal.org")
           "** %U :journal:\n%?")
          ("r" "to-read"
           entry
           (file+headline "~/Dropbox/org/to-read.org" "To File")
           "** %?  %^g\n %t")
          ("t" "Todo"
           entry
           (file+headline org-index-file "Tasks")
           "* TODO %^{Task} %^G\n %?")
          ("p" "Personal todo"
           entry
           (file+headline org-personal-file "general")
           "* TODO %^{Task} %^g\n %?")))

;;; Org Keybindings
  ;; Useful keybinds
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)

  ;; Hit C-c i to open up my todo list.
  (defun lp/open-index-file ()
    "Open the org TODO list."
    (interactive)
    (find-file org-index-file)
    (flycheck-mode -1)
    (end-of-buffer))

  (global-set-key (kbd "C-c i") 'lp/open-index-file)

  (defun lp/org-capture-todo ()
    (interactive)
    (org-capture :keys "t"))

  (defun lp/open-full-agenda()
    (interactive)
    (org-agenda :keys "n")
    (delete-other-windows))

  (global-set-key (kbd "M-n") 'lp/org-capture-todo)
  (global-set-key (kbd "<f1>") 'lp/open-full-agenda)


  ;; Auto wrap paragraphs in some modes (auto-fill-mode)
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  ;; sometimes i don't want to wrap text though, so we will toggle
  ;; with C-c q
  (global-set-key (kbd "C-c q") 'auto-fill-mode)

  ;; Clocking!
  (setq org-clock-into-drawer t)
  )


;; hugo because why not
(use-package ox-hugo
  :ensure t)
                                        ; research with org-mode!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pdf-tools init
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; org-ref
(use-package bibtex-utils
  :ensure t)

(use-package biblio
  :ensure t)

(use-package interleave
  :ensure t)
;;(require 'pubmed)
;;(require 'arxiv)
;;(require 'sci-id)

(autoload 'helm-bibtex "helm-bibtex" "" t)

(use-package org-ref
  :defer t
  :ensure t
  :config
  (require 'doi-utils)
  (setq org-ref-notes-directory "~/Dropbox/res"
        org-ref-bibliography-notes "~/Dropbox/res/notes.org"
        org-ref-default-bibliography '("~/Dropbox/res/index.bib")
        org-ref-pdf-directory "~/Dropbox/res/lib/"))

(use-package helm-bibtex
  :defer t
  :ensure t
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/res/index.bib" ;; where your references are stored
        helm-bibtex-library-path "~/Dropbox/res/lib/"
        bibtex-completion-library-path '("~/Dropbox/res/lib/") ;; where your pdfs etc are stored
        helm-bibtex-notes-path "~/Dropbox/res/notes.org" ;; where your notes are stored
        bibtex-completion-bibliography "~/Dropbox/res/index.bib" ;; completion
        bibtex-completion-notes-path "~/Dropbox/res/notes.org"))

(defun lp/open-paper-notes ()
  "Open the org TODO list."
  (interactive)
  (find-file "~/Dropbox/res/notes.org")
  (flycheck-mode -1))
(global-set-key  (kbd "C-c r") 'lp/open-paper-notes)

                                        ; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


                                        ; tex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(set-default 'preview-scale-function 2.0)
;; i don't know what this is
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; revert pdf-view after compilation
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-view-program-list
        '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq-default TeX-source-correlate-mode t))
                                        ; elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elfeed
  :ensure t
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
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defalias 'elfeed-toggle-star
;;   (elfeed-expose #'elfeed-search-toggle-all 'star))

;; (eval-after-load 'elfeed-search
;;   '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlights bad word choices and does stuff
(use-package writegood-mode
  :ensure t
  :defer t
  :hook text-mode
  :diminish writegood-mode
  :bind (("C-c g" . writegood-mode)
         ("\C-c\C-gg" . writegood-grade-level)
         ("\C-c\C-ge" . writegood-reading-ease))
  :config
  (add-to-list 'writegood-weasel-words "actionable"))


;; TODO - decide whether to use this or not
;; More badword highlighting! -


                                        ; toy areas of computer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lp/open-challenges-notes ()
  "Open the org TODO list."
  (interactive)
  (find-file "~/code/dailies/dailies.org")
  (flycheck-mode -1))

(global-set-key  (kbd "C-c y") 'lp/open-challenges-notes)


                                        ; w3m and internet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAB to jump from link to link.
;; RETURN to follow a link
;; SPACE to move down the page
;; b to move up the page
;; B to move back in the history
;; M to open the URL in Firefox
;; I to open the image if it didn’t show up correctly
;; c to copy the URL of the current page in the kill ring.
;; u to copy the URL of the link in the kill ring.
;; a to bookmark this page
;; v to look at the bookmarks
;; s to look through the page history for this session.

(use-package w3m
  :defer t
  :ensure t
  :commands w3m-goto-url w3m-search
  :init
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-cookies t)

  ;; clean up the w3m buffers:
  (add-hook 'w3m-display-functions 'w3m-hide-stuff)
  (add-hook 'w3m-mode 'ace-link-mode)

  (global-set-key (kbd "C-c w w") 'w3m-goto-url)
  (global-set-key (kbd "C-c w l") 'browse-url-at-point)
  (global-set-key (kbd "C-c w g") 'w3m-search)

  :config
  (define-key w3m-mode-map (kbd "&") 'w3m-view-url-with-external-browser))

(defun ha-switch-default-browser ()
  "Switches the default browser between the internal and external web browser."
  (interactive)
  ;;         | Variable                  | Function
  (if (equal browse-url-browser-function 'browse-url-default-browser)
      (if (fboundp 'w3m)
          (setq browse-url-browser-function 'w3m-browse-url)
        (setq browse-url-browser-function 'eww-browse-url))
    (setq browse-url-browser-function 'browse-url-default-browser))

  ;; Now we need to display the current setting. The variables are
  ;; pretty typical and have the goodies, but I just need to get rid
  ;; of the word "url" or "browser", and the results are pretty close:
  (cl-flet ((remove-bad-parts (l)
                              (-filter (lambda (s) (pcase s
                                                     ("url"     nil)
                                                     ("browse"  nil)
                                                     ("browser" nil)
                                                     (_  t))) l)))
    (message "Browser set to: %s"
             (-> (symbol-name browse-url-browser-function)
                 (split-string "-")
                 remove-bad-parts
                 car))))

(global-set-key (kbd "C-c w d") 'ha-switch-default-browser)

(defun w3m-skip-in-google ()
  "For a Google Search, skip to the first result."
  (beginning-of-buffer)
  (search-forward-regexp "[0-9, ]+ results")
  (forward-line 2)
  (recenter-top-bottom 0))
                                        ; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                                        ; weather
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package wttrin
  :defer t
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Swarthmore")))

                                        ; image manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package image+
  :ensure t
  :defer t
                                        ;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))


                                        ; TODO refile this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; theme stuff
;; The deeper blue theme is loaded but the resulting text
;; appears black in Aquamacs. This can be fixed by setting
;; the font color under Menu Bar->Options->Appearance->Font For...
;; and then setting "Adopt Face and Frame Parameter as Frame Default"
;; (use-package sourcerer-theme
;;   :ensure t)

;; (set-face-background 'hl-line "#372E2D")
;; ;; The minibuffer default colors with my theme are impossible to read, so change
;; ;; them to something better using ivy-minibuffer-match-face.
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((type tty) (background dark)) (:background "nil"))))
;;  '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
;;  '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
;;  '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
;;  '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
;;  '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(header-line ((t (:background "#003366"))))
;;  '(ivy-minibuffer-match-face-1 ((((class color) (background light)) (:background "#555555")) (((class color) (background dark)) (:background "#555555"))))
;;  '(ivy-minibuffer-match-face-2 ((t (:background "#314f30" :weight bold))))
;;  '(ivy-minibuffer-match-face-3 ((t (:background "#48225b" :weight bold))))
;;  '(ivy-minibuffer-match-face-4 ((t (:background "#680a0a" :weight bold))))
;;  '(which-func ((t (:foreground "#8fb28f")))))



;; get something up there for header
(which-function-mode t)

;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))

(defmacro with-face
    (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format

        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]")))
              )
        )
  )
;; Call the header line update
(add-hook 'buffer-list-update-hook
          'sl/display-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(ansi-term-color-vector
   [unspecified "#18262f" "#ef5253" "#7cc844" "#e4b51c" "#33b5e1" "#a363d5" "#33b5e1" "#a6afb8"] t)
 '(custom-enabled-themes (quote (base16-atelier-cave)))
 '(custom-safe-themes
   (quote
    ("304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "3be1f5387122b935a26e02795196bc90860c57a62940f768f138b02383d9a257" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "ed36f8e30f02520ec09be9d74fe2a49f99ce85a3dfdb3a182ccd5f182909f3ab" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "7a2ac0611ded83cdd60fc4de55ba57d36600eae261f55a551b380606345ee922" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "f6f5d5adce1f9a764855c9730e4c3ef3f90357313c1cae29e7c191ba1026bc15" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "7a1190ad27c73888f8d16142457f59026b01fa654f353c17f997d83565c0fc65" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "50b64810ed1c36dfb72d74a61ae08e5869edc554102f20e078b21f84209c08d1" "50ff65ab3c92ce4758cc6cd10ebb3d6150a0e2da15b751d7fbee3d68bba35a94" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "f2dd097452b79276ce522df2f8aeb37f6d90f504529616aa46122d549910e46d" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "ec3e6185729e1a22d4af9163a689643b168e1597f114e1cec31bdb1ab05aa539" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "69e7e7069edb56f9ed08c28ccf0db7af8f30134cab6415d5cf38ec5967348a3c" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "08f5da7e1f5064a2917af94f0dab946adfb25665b25450168ded749ec78a1145" "cde05ed51346d6925d29311fb131511115ae7612764297077ca1b61371e6b047" "c30d153e623dfe32184857790a0cad243b979e8b1104e057c4a6ffe2210856f7" "b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "25c06a000382b6239999582dfa2b81cc0649f3897b394a75ad5a670329600b45" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "30f7c9e85d7fad93cf4b5a97c319f612754374f134f8202d1c74b0c58404b8df" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" "8bb8a5b27776c39b3c7bf9da1e711ac794e4dc9d43e32a075d8aa72d6b5b3f59" "4b4cfb4e96e4a1c20416eeb16b1f90c895df31479a8255e01e671c503a48f707" "dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" "999d592328968aa33154e4e2385d53fd4c06b6ff60008fdadb682b07013f884c" "38b2a8441df2a4863bf5ca28648203ba0213d38f6630d3a7527828eb15f5a510" "616dc92e410a7f362757cb4dd3450bd650a69fd830cc2a7c73de2bdc90c526ad" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" "cfc62276fa8aa37e6567cf4b4502dfdb4995a2aaebc0dd9b9aee40383fa329c9" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "e1994cf306356e4358af96735930e73eadbaf95349db14db6d9539923b225565" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" default)))
 '(fci-rule-color "#14151E")
 '(line-spacing 0.2)
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (babel gnuplot company-yasnippet company-jedi ox-hugo org-edit-latex subatomic-theme avk-emacs-themes base16-theme autumn-light-theme hungry-delete moe-theme-switcher moe-theme cuda-mode google-c-style vlf clang-format preview auctex challenger-deep-theme emmet-mode page-break-lines yasnippet-snippets poet-theme artbollocks-mode image+ wttrin forecast web-mode espresso-theme comint w3m cyberpunk-theme doi-utils cherry-blossom-theme afternoon-theme auto-yasnippet eclipse-theme academic-phrases expand-region writegood-mode use-package tuareg smex slime rainbow-delimiters projectile powerline paredit org-ref org-link-minor-mode org-bullets multiple-cursors monokai-theme monokai-alt-theme merlin markdown-mode magit interleave ido-vertical-mode ido-completing-read+ helm-ag flycheck flx-ido elpy elfeed-org diminish diff-hl counsel bibtex-utils bibretrieve auto-compile ag adafruit-wisdom ace-window)))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type tty) (background dark)) (:background "nil"))))
 '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
 '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
 '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
 '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
 '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(header-line ((t (:background "#003366"))))
 '(ivy-minibuffer-match-face-1 ((((class color) (background light)) (:background "#555555")) (((class color) (background dark)) (:background "#555555"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "#314f30" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#48225b" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#680a0a" :weight bold))))
 '(which-func ((t (:foreground "#8fb28f")))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

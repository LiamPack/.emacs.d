(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")))
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; chicken before the
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; chicken before the
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(add-to-list 'load-path "/Users/sb/.emacs.d/diredp/")

(setf backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message nil
      wdired-allow-to-change-permissions t
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      ring-bell-function (lambda ()))

;; GUIs are for newbs
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Too distracting
(blink-cursor-mode -1)

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

;; Insert key is stupid
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      vc-follow-symlinks t)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)
                                        ;(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))

;; fill-column at 80 is the CS dept standard
;; Fill column + always show column
(setq fill-column 80)
(setq fci-rule-column 80)
(setq column-number-mode t)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)


(defun lp/kill-current-buffer ()
  "Just kill the gd buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'lp/kill-current-buffer)


(defun lp/generate-scratch-buffer ()
  "Generate random scratch buffer for whatever reason"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (emacs-lisp-mode))

(global-set-key (kbd "<f12>") 'lp/generate-scratch-buffer)


(defun eval-and-replace (value)
  "Evalute the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(global-set-key (kbd "C-c C-e") #'eval-and-replace)


(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))
(defun set-80-columns ()
  "Set the selected window to 80 columns. If given a prefix
    argument, set so that number of columns instead."
  (interactive)
  (set-window-width (or current-prefix-arg 80)))

(global-set-key (kbd "C-x ~") #'set-80-columns)


(defun slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;; Quick switch to scratch buffers
(defmacro scratch-key (key buffer-name mode)
  `(global-set-key ,key (lambda ()
                          (interactive)
                          (switch-to-buffer ,buffer-name)
                          (unless (eq major-mode ',mode)
                            (,mode)))))

(declare-function js2-mode nil)
(declare-function clojure-mode nil)
(scratch-key (kbd "C-c s") "*scratch*"    emacs-lisp-mode)
;;    (scratch-key (kbd "C-c j") "*javascript*" js2-mode)
;;    (scratch-key (kbd "C-c x") "*css*"        css-mode)
;;    (scratch-key (kbd "C-c h") "*html*"       html-mode)

(defun find-all-files (dir)
  "Open all files and sub-directories below the given directory."
  (interactive "DBase directory: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (cl-remove-if 'file-directory-p list))
         (dirs (cl-remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (find-file-noselect file))
    (dolist (dir dirs)
      (find-file-noselect dir)
      (find-all-files dir))))

(global-set-key (kbd "C-x C-S-d") 'find-all-files)

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))
    (whitespace-cleanup)))

(global-set-key (kbd "C-c n") 'indent-region-or-buffer)

(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "C-S-p") (lambda ()
                                (interactive)
                                (previous-line 3)))
(global-set-key (kbd "C-S-n") (lambda ()
                                (interactive)
                                (next-line 3)))

(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)
(setq global-mark-ring-max 50000)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") #'eval-buffer)
(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "<f5>") #'revert-buffer)

(use-package which-key
  :ensure t
  :disabled t
  :config (which-key-mode 1))

;; wrap visual lines! it helps.
(global-visual-line-mode 1)

;; Fancy lambdas
(global-prettify-symbols-mode t)

(when window-system
  (global-hl-line-mode))

;; iosevka term light, consolas, source code pro, Fira Code, dejavu, IBM 3270,
;; Fantasque Sans Mono, Terminus, overpass
(setq lp/default-font "overpass mono")
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

;; Hide a whole bunch of stuff on the modeline. It's a bit annoying.
;; Using the =diminish= package for this.
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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'default-black)
(use-package color-theme
  :ensure t)

(use-package color-theme-modern
  :after color-theme
  :ensure t
  :config
  (load-theme 'midnight))

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq mode-line-format (delq 'mode-line-position mode-line-format))
;;   (sml/setup)
;;   (sml/apply-theme 'light)
;;   (remove-hook 'display-time-hook 'sml/propertize-time-string)
;;   (setq sml/no-confirm-load-theme t))

;; Some minor modes menu thing? Not sure what it does.
(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))



(use-package dumb-jump
  :config
  (global-set-key (kbd "M-.") 'dumb-jump-go)
  (setq dumb-jump-selector 'ivy))

(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(setq ibuffer-use-other-window t) ;; always display ibuffer in another window

;; Buffer, Windows and Frames
(setq
 frame-resize-pixelwise t               ; Resize by pixels
 frame-title-format
 '(:eval (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name)) "%b"))
 ;; Size new windows proportionally wrt other windows
 window-combination-resize t)


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
;; You can also start by calling ace-window and then decide to switch the action to delete or swap etc. By default the bindings:
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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h l") 'counsel-find-library)
  (global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-h u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-'") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package smartparens
  :ensure t
  :config
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (add-hook 'c-mode #'smartparens-mode)
  (add-hook 'c++-mode #'smartparens-mode)
  (add-hook 'awk-mode #'smartparens-mode)
  (add-hook 'sh-mode #'smartparens-mode))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)



(use-package yasnippet
  :ensure t
  :functions yas-global-mode yas-expand
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-fallback-behavior 'return-nil)
  (setq yas-triggers-in-field t)
  (setq yas-verbosity 0)
  (yas-reload-all))

(use-package yasnippet-snippets ; more snippets!
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

(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer.
   try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
   try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
   try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
   try-complete-file-name ;; Try to complete text as a file name.
   try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
   try-expand-list ;; Try to complete the current line to an entire line in the buffer.
   try-expand-line ;; Try to complete the current line to an entire line in the buffer.
   try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
   try-complete-lisp-symbol) ;; Try to complete word as an Emacs Lisp symbol.
 )

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-,") 'er/expand-region))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

                                        ; org-mode
                                        ; TODO speed-keys?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-bullets
  :ensure t
  :config
  (setq org-ellipsis "⤵"))

;; (use-package ob-async
;;   :ensure t)

(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
         ("\C-cb" . org-iswitchb))
  :config
  (require 'org-habit)
  (unbind-key "C-," org-mode-map)       ;expand-region
  (unbind-key "C-'" org-mode-map)       ;avy

  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode)) )


  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-use-speed-commands t)
  ;; NOTE: If this isn't working, make sure to delete /
  ;; byte-recompile the /elpa/org/.. directory!
  ;; enable language compiles
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     (sh . t)
     (emacs-lisp . t)
     (gnuplot . t)
     ;;(ipython . t)
     (R . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (set-face-attribute 'org-block nil :background
                      (color-darken-name
                       (face-attribute 'default :background) 3))
  (setq org-src-window-setup 'current-window)
  ;;(setq ob-async-no-async-languages-alist '("ipython"))

  ;;;  file directory setup
  ;; Org-capture management + Tasks
  (setq org-directory "~/Dropbox/Org/")

  (defun org-file-path (filename)
    "Return absolute address of an org file give its relative name."
    (concat (file-name-as-directory org-directory) filename))

  ;; I'm pretty sure there's a better way to do this. probably just slap them in
  ;; a list and reduce from there
  (setq org-index-file (org-file-path "index.org"))
  (setq org-personal-file (org-file-path "personal.org"))
  (setq org-school-file (org-file-path "school.org"))
  (setq org-projects-file (org-file-path "projects.org"))
  (setq org-journal-file (org-file-path "journal.org"))
  (setq org-monthly-file (org-file-path "monthly.org"))
  (setq org-groceries-file (org-file-path "groceries.org"))
  (setq org-archive-location
        (concat (org-file-path "archive.org") "::* From %s"))

  ;; I keep all of my todos in =~/Dropbox/org/index.org= so I derive my
  ;; agenda from there

  (setq org-agenda-files
        (list org-index-file org-personal-file org-school-file
              org-projects-file
              org-journal-file (org-file-path "to-read.org")
              org-monthly-file org-groceries-file))
  (setq all-org-files
        (list org-index-file org-personal-file org-school-file
              org-projects-file org-journal-file
              org-monthly-file (org-file-path "to-read.org")
              org-groceries-file))

  ;; refiling!
  ;; I like to look at pretty much just up to 3 levels of targets
  (setq org-refile-targets '((all-org-files :maxlevel . 3)))

  ;; only look at top level headings. Since org-mode represents
  ;; these as files, this also means that the highest level heading
  ;; will be the first "file" so to speak
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; allow creating new parents on refile
  (setq org-refile-allow-creating-parent-nodes 'confirm)


  (setq to-read-tags '(":learning:" ":books:" ":emacs:" ":research:" ":manga:" ":anime:"
                       ":ml:" ":sites:" ":games:" ":music:" ":math:"))

  (defun lp/refile-to (file headline)
    "refile to specific spot (headline) in file"
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

  (defun lp/refile-to-file-with-tag (tag file headline)
    " Helper function to refile a group of tags to a certain file's headline"
    (while (not (equal nil (search-forward tag nil t)))
      (beginning-of-visual-line)
      (lp/refile-to file headline))
    (switch-to-buffer "index.org"))

  (defun lp/refile-school ()
    (lp/refile-to-file-with-tag ":school:" org-school-file "inbox"))

  (defun lp/refile-personal ()
    (lp/refile-to-file-with-tag ":personal:" org-personal-file "inbox"))

  (defun lp/refile-all-in-index ()
    (interactive)
    (beginning-of-buffer)
    (lp/refile-school)
    (beginning-of-buffer)
    (lp/refile-personal)
    (universal-argument) ;; universal argument is the C-u prefix!
    (save-some-buffers))

  (defun lp/refile-to-read ()
    " Invoke on headline of inbox in to-read.org. refiles all tagged entries to respective header"
    (interactive)
    ;; do for each tag in our "to-read" tags
    (dotimes (i (length to-read-tags))
      ;; Search forward until we can't anymore (no more items with this tag
      (let ((tag (nth i to-read-tags)))
        (save-excursion
          (while (not (equal nil (search-forward tag nil t)))
            (beginning-of-visual-line)
            (lp/refile-to (org-file-path "to-read.org") (substring tag 1 -1)))))
      ))

                                        ; todo stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "DeepSkyBlue1" :weight bold)
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

  ;; Place tags close to the right-hand side of the window
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)


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

                                        ; agenda stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-block-separator 45)
  ;; Check out NOX for stuff
  (require 'calendar)

  (defun jtc-org-tasks-closed-in-month (&optional month year match-string)
    "Produces an org agenda tags view list of the tasks completed
in the specified month and year. Month parameter expects a number
from 1 to 12. Year parameter expects a four digit number. Defaults
to the current month when arguments are not provided. Additional search
criteria can be provided via the optional match-string argument "
    (interactive)
    (let* ((today (calendar-current-date))
           (for-month (or month (calendar-extract-month today)))
           (for-year  (or year  (calendar-extract-year today))))
      (org-tags-view nil
                     (concat
                      match-string
                      (format "+CLOSED>=\"[%d-%02d-01]\""
                              for-year for-month)
                      (format "+CLOSED<=\"[%d-%02d-%02d]\""
                              for-year for-month
                              (calendar-last-day-of-month for-month for-year))))))

  (defun jtc-foo-tasks-last-month ()
    "Produces an org agenda tags view list of all the tasks completed
last month with the Category Foo."
    (interactive)
    (let* ((today (calendar-current-date))
           (for-month (calendar-extract-month today))
           (for-year  (calendar-extract-year today)))
      (calendar-increment-month for-month for-year -1)
      (jtc-org-tasks-closed-in-month
       for-month for-year "+TODO=\"DONE\"")))

  ;; AGENDA
  (setq-default
   org-agenda-custom-commands
   '(("n" "Agenda"
      ((agenda ""
               ((org-agenda-files (list org-index-file
                                        org-personal-file org-school-file
                                        org-projects-file org-journal-file
                                        org-monthly-file org-groceries-file
                                        ))
                (org-agenda-skip-scheduled-if-deadline-is-shown t)))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!-DONE-HOLD"
                  ((org-agenda-overriding-header "To-File Files (index.org)")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-files (list org-index-file))))
       (tags "cs73|cs87|research|cs"
             ((org-agenda-overriding-header "CS Work")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       ;; (tags "jpns" ----- rip jpns..
       ;;       ((org-agenda-overriding-header "JPNS")
       ;;        (org-tags-match-list-sublevels nil)
       ;;        (org-agenda-files (list org-school-file))))
       (tags "physics"
             ((org-agenda-overriding-header "Physics")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags "math"
             ((org-agenda-overriding-header "Math")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags "kizuna|smash|outsiders"
             ((org-agenda-overriding-header "Clubs")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-school-file))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Personal Stuff")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-files (list org-personal-file))))))

     ("t" "To Read Stuff"
      ((tags-todo "music/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Music")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "anime/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Anime")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "sites/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Sites ")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "research/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Research Papers")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "manga/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Manga")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "learning/!-DONE-HOLD"
                  ((org-agenda-overriding-header "Things to Learn")
                   (orgs-tags-match-list-sublevels nil)
                   (org-agenda-files (list (org-file-path "to-read.org")))))
       (tags-todo "books-learning/!-DONE-HOLD-WAITING"
                  ((org-agenda-overriding-header "Books")
                   (orgs-tags-match-list-sublevels nil)
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
                                        ;org-agenda-time-grid '((daily today require-timed) nil "......" "----------------")
   )
  ;; Custom agenda command definitions
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
  (setq org-log-done 'time)   ; also record when the TODO was archived

  (setq org-capture-templates
        '(("g" "Groceries"
           entry
           (file "~/Dropbox/org/groceries.org")
           "- [ ] %?\n")
          ("i" "Ideas"
           entry
           (file+headline "~/Dropbox/org/ideas.org" "Project Ideas")
           "** [#%^{9}] %?\n")
          ("j" "Journal"
           entry
           (file+datetree "~/Dropbox/org/journal.org")
           "** %U :journal:\n%?\n good things that happened today?\n")
          ("t" "to-read"
           entry
           (file+headline "~/Dropbox/org/to-read.org" "inbox")
           "** TODO %^{to-read}  %^g\n %U")
          ("z" "Todo"
           entry
           (file+headline org-index-file "Tasks")
           "* TODO %^{Task} %^G\n %U\n%?")
          ("p" "Personal todo"
           entry
           (file+headline org-personal-file "general")
           "* TODO %^{Task} %^g\n %?")))

  ;;; Org Keybindings
  ;; Useful keybinds
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)

  (defun lp/org-capture-todo ()
    (interactive)
    (org-capture :keys "z"))

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

  ;; Hit C-c i to open up my todo list.
  (defun lp/open-index-file ()
    "Open the org TODO list."
    (interactive)
    (find-file org-index-file)
    (flycheck-mode -1)
    (end-of-buffer))

  (global-set-key (kbd "C-c i") 'lp/open-index-file))

;; org-ref
(use-package interleave
  :ensure t)
;;(require 'pubmed)
;;(require 'arxiv)
;;(require 'sci-id)

(use-package recentf
  :ensure t
  :config
  (recentf-mode)
  (setq
   recentf-max-menu-items 15
   recentf-max-saved-items 200
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         ;; And all other kinds of boring files
                         #'ignoramus-boring-p)))

(use-package vlf
  :ensure t
  :config
  (setq vlf-application 'dont-ask) ; please don't ask wehn you open a big file
  )

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq c-default-style "linux" ; set style to "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook 'hs-minor-mode)

(setq gdb-many-windows t        ; use gdb-many-windows by default
      gdb-show-main t)          ; Non-nil means display source file containing the main routine at startup

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
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package company-jedi
  :ensure t
  :disabled t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; (use-package pyenv-mode
;;   :ensure t)

;; (use-package haskell-mode
;;   :ensure t
;;   :hook (haskell-mode . #'hident-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit))

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

(dolist (hook lisp-mode-hooks)
  (add-hook hook (lambda ()
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(use-package iedit
  :ensure t
  :config
  (defun ap/iedit-mode (orig-fn)
    "Call `iedit-mode' with function-local scope by default, or global scope if called with a universal prefix."
    (interactive)
    (pcase current-prefix-arg
      ('nil (funcall orig-fn '(0)))
      ('(4) (funcall orig-fn))
      (_ (user-error "`ap/iedit-mode' called with prefix: %s" prefix))))

  ;; Override default `iedit-mode' function with advice.
  (advice-add #'iedit-mode :around #'ap/iedit-mode)

  (global-set-key (kbd "C-:") #'iedit-mode))


(require 'eshell)
(require 'em-alias)
(require 'cl)

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

                              (add-to-list 'eshell-visual-commands "ssh")
                              (add-to-list 'eshell-visual-commands "tail")
                              (add-to-list 'eshell-visual-commands "htop")
                              (add-to-list 'eshell-visual-commands "vim")
                              (setq eshell-visual-subcommands '("git" "log"
                                                                "l" "diff" "show"))
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

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

;; change listing switches based on OS
(when (not (eq system-type 'windows-nt))
  (eshell/alias "ls" "ls --color -h --group-directories-first $*"))

(setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
 eshell-scroll-to-bottom-on-input 'all
 eshell-error-if-no-glob t
 eshell-hist-ignoredups t
 eshell-save-history-on-exit t
 eshell-prefer-lisp-functions nil
 eshell-destroy-buffer-when-process-dies t)

;; these two are used to make org mode look hella nice.
(use-package visual-fill-column
  :ensure t)

(use-package writeroom-mode ; TODO make a bind for this mode in org-mode
  :ensure t
  :defer t
  :after visual-fill-column
  :diminish writeroom-mode)

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


;; quiclisp + slime
(defun ert-all ()
  (interactive)
  (ert t))

(defun ielm-repl ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(define-key emacs-lisp-mode-map (kbd "C-x r")   #'ert-all)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
;;(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer*)
(defalias 'lisp-interaction-mode 'emacs-lisp-mode)

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(\\(\\(?:\\(?:\\sw\\|\\s_\\)+-\\)?"
             "def\\(?:\\sw\\|\\s_\\)*\\)\\_>"
             "\\s-*'?" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
    (1 'font-lock-keyword-face)
    (2 'font-lock-function-name-face nil t)))
 :low-priority)

(use-package slime ; slime for our clisp goodness
  :ensure t
  :config
  (setq inferior-lisp-program "clisp.exe")
  (load "C:\\quicklisp\\slime-helper.el")
  (slime-setup '(slime-repl))
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

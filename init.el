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

;; fuck cursor lagging on moving
(setq auto-window-vscroll nil)

;; paren mode
(setq show-paren-mode t)

;; Disable window chrome
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))
(setq inhibit-startup-message t
      initial-scratch-message (format ";; %s\n" (adafruit-wisdom-select)))

;; stop truncating lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)


;; powerline is terrific
(use-package powerline
  :ensure t
  :config (powerline-center-theme))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))


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
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

;; Font functionality
                                        ; iosevka, consolas, source code pro, Fira Code
(setq lp/default-font "Fira Code")
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

;; linum mode with spaces? TODO / WIP
;; (global-linum-mode 1)
;; (defadvice linum-update-window (around linum-dynamic activate)
;;   (let* ((w (length (number-to-string
;;                      (count-lines (point-min) (point-max)))))
;;          (linum-format (concat " %" (number-to-string w) "d ")))
;;     ad-do-it))
;;(setq fci-rule-color (face-attribute 'linum :foreground))

;; Fill column
(setq fci-rule-column 80)


;; global-hl-line-mode softly highlights bg color of line. Its nice.
(when window-system
  (global-hl-line-mode))

                                        ; useful functions + keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                                        ;(global-set-key (kbd "M-o") 'other-window)

;; Gotta keep those buffers clean
(global-set-key (kbd "C-c n") 'lp/cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'lp/cleanup-buffer)

;; Open up a randomly generated scratch buffer (cause i like scratch buffers)
(global-set-key (kbd "<f12>") 'lp/generate-scratch-buffer)

;; Compile on a keybind
(global-set-key (kbd "C-<f7>") 'compile)

;; Quickly comment region
(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-region)

;; fuk these defaults
(global-set-key (kbd "M-g") #'goto-line)

;; pop to the last command mark! its cool.
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

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
  (progn
    (setq  aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))



;;; Hide a whole bunch of stuff on the modeline. It's a bit annoying.
;;; Using the =diminish= package for this.
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
(diminish-minor-mode 'paredit 'paredit-mode " π")
(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")




                                        ; lisps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I'd prefer that compilation output goes to *compilation buffer*
;;; Rarely have the window selected, so the output disappears past the
;;; bottom of the window
(setq compilation-scroll-output t)
(setq compilation-window-height 15)


;; quicktramp setup
(setq tramp-default-method "ssh")

;; We want all lispy languages to use =paredit-mode= and =rainbow-delimiters
(setq lisp-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook)) ; can add more or whatever


(dolist (hook lisp-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))
                                        ; (add-hook 'text-mode-hook 'hook-function)


                                        ; programming environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
;; God bless magit and all that it does
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-push-always-verify nil))

;;; Dired
;; clean up permissions and owners, less noisy
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))
;; disable ls by default
(setq dired-use-ls-dired nil)

;;; Projectile ! TODO - use-package this

(require 'projectile)

;; Projectile everywhere obviously
(projectile-global-mode)

(defun lp/search-project-for-symbol-at-point ()
  "Use projectile-ag to search current project for the current symbol."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))
(global-set-key  (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'lp/search-project-for-symbol-at-point)

;;; Currently using eshell for my shell sessions. Bound to C-c s.
(global-set-key (kbd "C-c s") 'eshell)

;;; A quick hook to C modes to quickswap to =.h= files or =.c= files. It's nice
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
;; also gdb is cool
(setq gdb-many-windows 't)

;; Slime for lisps!
(use-package slime
  :ensure t
  :config
  (progn
    (slime-setup '(slime-repl))
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (elpy-enable)
    (setq python-indent-offsett 2)))



;; flycheck mode is not too bad.
(use-package flycheck
  :ensure t
  :disabled
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'jc/use-eslint-from-node-modules)
  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

;; Yasnippet configuration
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (yas-global-mode 1)
    (setq yas-fallback-behavior 'return-nil)
    (setq yas-triggers-in-field t)
    (setq yas-verbosity 0)
    (setq yas-snippet-dirs (list "~/.emacs.d/snippets/" "~/.emacs.d/elpa/yasnippet-20170923.1646/snippets/"))
    ;; (define-key yas-minor-mode-map [(tab)] nil)
    ;; (define-key yas-minor-mode-map (kbd "TAB") nil)
    ))

;; Ido configuration
(use-package flx-ido
  :disabled
  :ensure t
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)
    (ido-ubiquitous-mode 1)
    (flx-ido-mode 1) ; better/faster matching
    (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

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
  (progn
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))

    (setq ivy-use-virtual-buffers t)
    (setq ivy-initial-inputs-alist nil)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "%d/%d ")
    (setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))))

;; sick of this blinking
(blink-cursor-mode -1)


(use-package counsel
  :ensure t
  :bind (("C-x f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-x" . counsel-M-x)
         ("C-c o" . counsel-recentf)
         ("C-x l" . counsel-locate))
  :config
  (progn
    (setq counsel-find-file-at-point t)))

(use-package swiper
  :ensure t
  ;; :bind (:map isearch-mode-map
  ;;             ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind (("C-c C-r" . swiper)
         ("C-c C-s" . counsel-grep-or-swiper))
  )
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
  (progn
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
    (add-hook 'multiple-cursors-mode-disabled-hook #'modi/mc-when-disabled)))


                                        ; org-mode
                                        ; TODO - speed-keys?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'org)
(require 'org-bullets)
(use-package org-bullets
  :hook org
  :config
  (setq org-ellipsis "⤵"))
;; Pretty bullets are better than list of asterisk
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-M-RET-may-split-line nil)

;; When editing code snippet/ block, use syntax highlighting for that language
;; Also don't open new window for src blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-src-window-setup 'current-window)
(add-to-list 'org-babel-load-languages '(emacs-lisp . t))
(add-to-list 'org-babel-load-languages '(dot . t))
(add-to-list 'org-babel-load-languages '(ditaa . t))
(add-to-list 'org-babel-load-languages '(ipython . t))
(add-to-list 'org-babel-load-languages '(python . t))
(add-to-list 'org-babel-load-languages '(C . t))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t) (python . t) (emacs-lisp . t) (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)
;; Org-capture management + Tasks
(setq org-directory "~/Dropbox/org/")

(defun org-file-path (filename)
  "Return absolute address of an org file give its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-personal-file (org-file-path "personal.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))


;; I keep all of my todos in =~/org/index.org= so I derive my agenda from there
(setq org-agenda-files (list org-index-file org-personal-file))

;; Bind C-c C-x C-s to mark todo as done and archive it
(defun lp/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it"
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))
(define-key org-mode-map (kbd "C-c C-x C-s") 'lp/mark-done-and-archive)
(setq org-log-done 'time) ; also record when the TODO was archived

(setq org-capture-templates
      '(("r" "to-read"
         checkitem
         (file "~/Dropbox/org/to-read.org"))
        ("i" "Ideas"
         entry
         (file "~/Dropbox/org/ideas.org")
         "* %?\n")
        ("j" "Journal"
         entry
         (file "~/Dropbox/org/journal.org")

         "** %u :journal: \n %?")
        ("t" "Todo"
         entry
         (file+headline org-index-file "Tasks")
         "* TODO %?\n")
        ("p" "Personal todo"
         entry
         (file+headline org-personal-file "general")
         "* TODO %?\n")))

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

;; sometimes i don't want to wrap text though, so we will toggle with C-c q
(global-set-key (kbd "C-c q") 'auto-fill-mode)


                                        ; research with org-mode!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pdf-tools init
(pdf-tools-install)

;; org-ref
(require 'org-ref)
(require 'bibtex-utils)
(require 'biblio)
(require 'helm-bibtex)
(require 'doi-utils)
;;(require 'pubmed)
;;(require 'arxiv)
;;(require 'sci-id)

(autoload 'helm-bibtex "helm-bibtex" "" t)

(use-package org-ref
  :config
  (setq org-ref-notes-directory "~/Dropbox/res"
        org-ref-bibliography-notes "~/Dropbox/res/notes.org"
        org-ref-default-bibliography '("~/Dropbox/res/index.bib")
        org-ref-pdf-directory "~/Dropbox/res/lib/"))

(use-package helm-bibtex
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/res/index.bib" ;; where your references are stored
        helm-bibtex-library-path "~/Dropbox/res/lib/"
        bibtex-completion-library-path '("~/Dropbox/res/lib/") ;; where your pdfs etc are stored
        helm-bibtex-notes-path "~/Dropbox/res/notes.org" ;; where your notes are stored
        bibtex-completion-bibliography "~/Dropbox/res/index.bib" ;; writing completion
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



                                        ; elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ("E" . bjm/elfeed-show-emacs)
              ("D" . bjm/elfeed-show-daily)))
(global-set-key (kbd "C-x w") 'elfeed)
(setq shr-width 80)

(setq-default elfeed-search-filter "@2-weeks-ago +unread ")
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))


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
  '(define-key elfeed-search-mode-map (kbd "8") 'bjm/elfeed-unstar))


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
  :bind (("C-c g" . writegood-mode)
         ("\C-c\C-gg" . writegood-grade-level)
         ("\C-c\C-ge" . writegood-reading-ease))
  :config
  (add-to-list 'writegood-weasel-words "actionable"))


                                        ; toy areas of computer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lp/open-challenges-notes ()
  "Open the org TODO list."
  (interactive)
  (find-file "~/code/personal/dailies/dailies.org")
  (flycheck-mode -1))

(global-set-key  (kbd "C-c y") 'lp/open-challenges-notes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#d6d6d6" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#4d4d4c"))
 '(beacon-color "#c82829")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai-alt)))
 '(custom-safe-themes
   (quote
    ("c4c2c9c728c6c16d7155c3851ae7309446bee8b5eb5973d9cccb9f7a178d55ff" "d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default)))
 '(fci-rule-color "#3C3D37")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(org-agenda-tags-column 80)
 '(package-selected-packages
   (quote
    (ace-window multiple-cursors counsel helm-ag elpy writegood-mode markdown-mode flycheck adafruit-wisdom auto-compile color-theme-solarized color-theme ivy swiper ag monokai-alt-theme monokai-theme elfeed-org elfeed color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow interleave org-ref pdf-tools tuareg merlin slime powerline moe-theme f auctex ido-vertical-mode flx-ido ido-ubiquitous yasnippet org-bullets rainbow-delimiters projectile paredit org-link-minor-mode magit diminish diff-hl)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(send-mail-function (quote mailclient-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue")))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

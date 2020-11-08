;;; init.el --- -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/etc")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; TODOs for the config: (to fix check out github.com/alhassy/emacs.d

(require 'package)
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
(package-initialize)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
;; chicken before the
(eval-when-compile
  (require 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)


;; (require 'cygwin-mount)

(setenv "PATH" (concat "/usr/bin" ";" (getenv "PATH")))

(require 'dash)
(use-package diminish
  :ensure t)
(use-package s
  :ensure t)
;; ok enough of the use-p setup, time to get -sensible- (thanks hrs)
;; (require 'sensible-defaults)
;; (sensible-defaults/use-all-keybindings)
;; (sensible-defaults/use-all-settings)

;; Emacs Server
(server-start)

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
      ring-bell-function (lambda ())
      custom-safe-themes t)

;;  GUIs :(
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(display-time-mode t)
(set-face-attribute 'default nil :height 180)
(set-frame-font "Input" nil t)

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
      scroll-conservatively most-positive-fixnum
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


;;; --- my keybindings and the like
;; Some more sensible keybindings
;; Please kill the damn buffer
(global-set-key (kbd "C-x k")
                '(lambda () (interactive) (kill-buffer (current-buffer))))

                                        ; Please clean up the buffer
(defun lp/cleanup-buffer-safe ()
  "Perform some safe operations to remove garbage whitespace content"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(defun lp/cleanup-buffer ()
  (interactive)
  (lp/cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'lp/cleanup-buffer)

;; Open buffer and switch (and swapping:)
(defun lp/split-window-right-and-switch ()
  "Split window vertically and switch"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(defun lp/split-window-right-and-switch-other-buffer ()
  "Split window vertically and switch"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (switch-to-buffer (other-buffer)))
(defun lp/split-window-below-and-switch ()
  "Split window horizontally and switch"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(defun lp/switch-2w-horizontal-vertical ()
  "Switch a horizontal 2-window setup to vertical."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "can't toggle with too many windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
(global-set-key (kbd "C-x C-1") 'lp/switch-2w-horizontal-vertical)
(global-set-key (kbd "C-x 2") 'lp/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'lp/split-window-right-and-switch)
(global-set-key (kbd "C-x C-3") 'lp/split-window-right-and-switch-other-buffer)
(global-set-key (kbd "<f5>") #'revert-buffer)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(100 95))
;; (add-to-list 'default-frame-alist '(alpha 100 95))

;;; --- whichkey mode!
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; --- Navigation and Window Management (helm + soontobe posframe)
;; Window management
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; posframe for when it works someday
(use-package posframe
  :ensure t
  :diminish posframe-mode)

(use-package helm
  :disabled
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-c i" . helm-imenu)
         ("C-c h x" . 'helm-register)
         ("C-c g" . helm-google-suggest))
  :diminish helm-mode
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)

  ;; helm is a little much for me
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)

  (with-eval-after-load 'helm
    (setq helm-always-two-windows nil)
    (setq helm-display-buffer-default-height 15)
    (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))

  ;; TODO => get this actually working
  ;;
  ;; Current bug: after exiting helm, (helm-cleanup) forces emacs to suspend or
  ;; close or something. Something in the helm-cleanup-hooks is doing weird
  ;; stuff on a condition check that's returning a ~nil~. Can't tell
  ;;
  ;; if you check the github issues on this package, the author says the safest
  ;; way to deal with it is a -defadvice-. Zulu-inuoe foudn that it was the
  ;; bury-buffer in helm.el#L3960. Can fix this I guess.
  (use-package helm-posframe
    :ensure t
    :config
    (helm-posframe-enable)
    ;; Check posframe.el:L222
    (setq helm-posframe-poshandler #'posframe-poshandler-frame-bottom-right-corner))

  ;; a surprisingly sick package that I haven't used before. Can multi-search
  ;; all buffers or whatever you're looking for (swoops in).
  ;; TODO => Need to set the variable that favors efficiency over coloring
  (use-package helm-swoop
    :ensure t
    :bind (("C-c C-s" . helm-swoop)))
  (use-package helm-ag
    :ensure t
    :config
    (setq helm-ag-base-command "C:/msys64/mingw64/bin/ag --nocolor --nogroup")))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (use-package ivy-posframe
    :ensure t
    :diminish ivy-posframe-mode
    :config
    (require 'ivy-posframe)
    ;; display at `ivy-posframe-style'
    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
    ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
    (ivy-posframe-mode 1))

  (use-package counsel
    :ensure t
    :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "C-h v") 'counsel-describe-variable)
    (global-set-key (kbd "C-h o") 'counsel-describe-symbol)
    (global-set-key (kbd "C-h l") 'counsel-find-library)
    (global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "C-h u") 'counsel-unicode-char)
    ;; (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-c g") 'counsel-google)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (setq counsel-ag-base-command "/mingw64/bin/ag --vimgrep %s"))

  (use-package swiper
    :ensure t
    :config
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key (kbd "C-c C-s") 'swiper-isearch)
    (global-set-key (kbd "C-s") 'isearch-forward-regexp))


  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (ivy-mode 1)
  )


(use-package wgrep
  :ensure t
  :config
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))


;;; --- recentf
(use-package recentf
  :ensure t
  :diminish recentf-mode
  :config
  (recentf-mode)
  (setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15
   ;; Cleanup recent files only when Emacs is idle, but not when the mode
   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
   ;; idles often enough to have the recent files list clean up regularly
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "/itsalltext/"      ; It's all text temp files
                         ;; And all other kinds of boring files
                         #'ignoramus-boring-p)))


;;; --- avy!
(use-package avy
  :ensure t
  :config
  ;; bind an avy version in the isearch minibuffer
  (eval-after-load "isearch"
    '(define-key isearch-mode-map (kbd "C-\"") 'avy-isearch))
  (global-set-key (kbd "C-'") 'avy-goto-char-timer))


;;; --- company
(use-package company
  :diminish
  :after elm
  :config
  (setq company-global-modes '(not eshell-mode))
  (global-company-mode 1)
  (define-key company-active-map (kbd "<backtab>") #'company-complete-selection)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (add-to-list 'company-backends 'elm-company)
  (setq ;; Only 2 letters required for completion to activate.
   company-minimum-prefix-length 2

   ;; Search other buffers for completion candidates
   company-dabbrev-other-buffers t
   company-dabbrev-code-other-buffers t

   ;; Do not downcase completions by default.
   company-dabbrev-downcase nil

   ;; Even if I write something with the ‘wrong’ case,
   ;; provide the ‘correct’ casing.
   company-dabbrev-ignore-case t

   ;; don't require matching input please
   company-require-match 'never

   ;; Immediately activate completion.
   company-idle-delay 0))


;;; --- evil
(use-package evil
  :ensure t
  :diminish evil-mode
  :config
  (evil-mode)
  (setf evil-ex-search-highlight-all nil)
  (add-to-list 'evil-normal-state-modes 'sly-db-mode)
  ;; Make it easier to exit insert-mode
  (use-package key-chord
    :ensure t)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1)

  ;; TODO => Actually start using the keymap
  (defvar my-leader-map (make-sparse-keymap)
    "A quick keymap for vim leader key shortcuts")
  (global-undo-tree-mode -1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))


;;; --- aesthetics
;; Make sure all other themes are disabled when loading a theme
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(setq my-themes '(doom-acario-dark
                  doom-tomorrow-night
                  base16-porple
                  solarized-light
                  vscode-dark-plus
                  github-modern
                  kaolin-galaxy
                  gruber-darker
                  srcery
                  modus-operandi
                  modus-vivendi))

(setq theme-index (1- (length my-themes))) ; start at solarized I guess
(load-theme (nth theme-index my-themes) t)
(defun cycle-theme-n (n)
  (setq theme-index (mod (+ theme-index n) (length my-themes)))
  (load-theme (nth theme-index my-themes) t))
(defun cycle-theme-forward ()
  (interactive)
  (cycle-theme-n 1))
(defun cycle-theme-backward ()
  (interactive)
  (cycle-theme-n -1))
(global-set-key (kbd "C-c t") #'cycle-theme-forward)
(global-set-key (kbd "C-c T") #'cycle-theme-backward)

;; some nice theme packages
(use-package doom-themes
  :ensure t)
(use-package base16-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)

;;;; --- programming stuff incoming
;;; --- Snippets :) (mostly from https://github.com/alhassy/emacs.d#snippets----template-expansion)

;; Yet another snippet extension program
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1) ;; Always have this on for when using yasnippet syntax within yankpad
  ;; respect the spacing in my snippet declarations
  (setq yas-indent-line 'fixed))

;; Alternative, Org-based extension program
;; Tutorial / docs here https://github.com/Kungsgeten/yankpad
(use-package yankpad
  :ensure t
  :diminish
  :config
  ;; Location of templates
  (setq yankpad-file "~/.emacs.d/yankpad.org")

  ;; Ignore major mode, always use defaults.
  ;; Yankpad will freeze if no org heading has the name of the given category.
  (setq yankpad-category "Default")

  ;; Load the snippet templates ---useful after yankpad is altered
  (yankpad-reload)

  ;; Set company-backend as a secondary completion backend to all existing backends.
  ;;(setq company-backends (mapcar #'my/company-backend-with-yankpad company-backends))
  )


;;; --- Version Control (magit stuff :>)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package projectile
  :ensure t
  :config

  (use-package counsel-projectile
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (counsel-projectile-mode)))

;;; --- Julia
(use-package julia-mode
  :ensure t)
(use-package julia-repl
  :ensure t)

;;; --- elm
(use-package elm-mode
  :ensure t
  :hook ((elm-mode . elm-format-on-save-mode))
  :init
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode)
  (setq elm-indent-offset 2)

  )


;;; --- haskell
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-type 'cabal-new-repl)
  (setq haskell-process-log t)

  (add-hook 'haskell-mode-hook 'haskell-indent-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode))

;;; --- Lisps
;; general lisp config
(use-package flycheck
  :ensure t
  :config
  (flycheck-mode))

(use-package lsp-haskell
  :ensure t)
(use-package lsp-ivy
  :ensure t)
(use-package lsp-mode
  :ensure t
  :after (lsp-haskell lsp-ivy)
  :hook (((python-mode haskell-mode cc-mode) . (lambda () (lsp))))
  :config
  (use-package lsp-ui
    :ensure t
    :config)
  )
(use-package paredit
  :ensure t
  :diminish paredit-mode)
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode)
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode)
(use-package racket-mode
  :ensure t)

(use-package sly
  :ensure t
  :bind (:map sly-mode-map
              ("C-c C-b" . sly-eval-buffer))
  :config

  ;; check dpkg -L sbcl-source to find where the source stuff is
  ;; Then you need to add the line
  ;;    (sb-ext:set-sbcl-source-location "/usr/share/sbcl-source/")
  ;; to your ~/.sbclrc
  (setq inferior-lisp-program "sbcl")
  (setq sly-contribs '(sly-fancy))
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

(use-package sly-quicklisp
  :ensure t)

(setq lisp-mode-hooks '(clojure-mode-hook
                        emacs-lisp-mode-hook
                        lisp-mode-hook
                        scheme-mode-hook
                        racket-mode-hook
                        sly-mode-hook))

(dolist (hook lisp-mode-hooks)
  (add-hook hook (lambda ()
                   (paredit-mode)
                   (rainbow-delimiters-mode)
                   (aggressive-indent-mode))))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)
(diminish 'eldoc-mode)

;;; -- Oh boy here comes the org mode
(use-package org-bullets
  :ensure t
  :config
  (setq org-ellipsis "⤵"))

(use-package org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . org-bullets-mode))
  :init
  (require 'ox-latex)
  :custom
  (org-pretty-entities t)               ; ligature displaying
  (prety-entities-include-sub-superscripts t)
  (org-hide-block-startup t)
  (org-list-allow-alphabetical t)
  (org-catch-invisible-edits 'show)
  (org-use-speed-commands t)
  (org-use-fast-todo-selection t) ; pops open quick dialogue for selecting state
  (org-treat-S-cursor-todo-selection-as-state-change nil) ; using S-left/right
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(latex))
  (org-log-done 'time)
  (org-todo-keywords
   '((sequence "TODO(t)" "(STARTED(s@/!)" "|" "DONE(d/!)")
     (sequence "WAITING(w@/!)" "ON_HOLD(h@/!)" "|" "CANCELLED(c@/!)")))  :config
  (setq org-todo-keyword-faces
        '(("TODO"       :foreground "red"               :weight bold)
          ("STARTED"    :foreground "bold"              :weight bold)
          ("DONE"       :foreground "forest green"      :weight bold)
          ("WAITING"    :foreground "orange"            :weight bold)
          ("ON_HOLD"    :foreground "magenta"           :weight bold)
          ("CANCELLED"  :foreground "forest green"      :weight bold))) ;; free up the avy conflict I think
  :config
  (unbind-key "C-'" org-mode-map)       ;avy

  (setq org-latex-listings 'minted)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-enforce-todo-dependencies t)
  ;; src blocks
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)))

  ;; Setup for org file shenanigans
  (setq org-directory "~/Dropbox/Org")
  (defun org-file-path (filename)
    "Return absolute path of one of my dropbox files from its relative name"
    (concat (file-name-as-directory org-directory) filename))

  (setq org-agenda-files '())

  ;; Show that agenda man
  (defun show-agenda ()
    (interactive)
    (org-agenda nil "a"))
  (global-set-key (kbd "<f1>") #'show-agenda)

  (use-package doct
    :ensure t
    :commands (doct))

  (require 'doct)
  (setq org-capture-templates
        (doct '(("Inbox" :keys "c"
                 :file "~/Dropbox/Org/Inbox.org"
                 :type entry
                 :headline "Inbox"
                 :todo-state "TODO"
                 :template ("* %{todo-state} %^{Description}"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?"))
                ("Project Planning" :keys "p"
                 :file "~/Dropbox/Org/Project-Planning.org"
                 :type entry
                 :headline "Refile"
                 :todo-state "TODO"
                 :template ("* %{todo-state} %^{Description} %^g"
                            ":PROPERTIES:"
                            ":Created: %U"
                            ":END:"
                            "%?")))))

  (defun website-style ()
    "docstring"
    (interactive)
    (let ((current-theme (car custom-enabled-themes)))
      (load-theme 'leuven t)
      (org-publish "blog")
      (load-theme current-theme)))

  (global-set-key (kbd "C-c <f8>") 'website-style)

  ;; Posts that helped to set this up
  ;; Blogging with Emacs -- https://bastibe.de/2013-11-13-blogging-with-emacs.html
  ;; Some guy's blog config -- https://github.com/DiegoVicen/my-emacs#my-blog-publishing-configuration
  ;; CSS theme from here https://gongzhitaao.org/orgcss/
  (setq org-publish-project-alist
        '(("blog-notes"
           :base-directory "~/projects/website/org"
           :base-extension "org"
           :publishing-directory "~/projects/website/public"
           :recursive t
           :publishing-function org-html-publish-to-html
           :with-toc nil
           :with-creator nil
           :headline-levels 4
           :section-numbers nil
           :html-head nil
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           ;; From https://bastibe.de/
           :html-head-extra
           "
          <title>LPac's Pages</title>
          <meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
          "
           :html-preamble
           "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/org.css\"/>
           <link rel=\"stylesheet\" type=\"text/css\" href=\"../css/org.css\"/>
            <div class=\"header\">
              <div class=\"site-title\"><a href=\"/\">LPac's Pages</a></div>
              <nav class=\"page-nav\">
                  <ul>
<li><a href=\"/\">home</a>  </li>
<li><a href=\"http://github.com/liampack\">Github</a> </li>
<li><a href=\"/archive.html\">Other posts</a> </li>
<li><a href=\"/posts/favorites.html\">favorites</a> </li>
<li><a href=\"/posts/least-favorites.html\">least favorites</a></li>
                  </ul>
              </nav>
          </div>"
           :html-postamble
           "<div id=\"archive\"><a href=\"/archive.html\">Other posts</a></div>"
           ;; sitemap - list of blog articles
           :auto-sitemap t
           :sitemap-filename "archive.org"
           :sitemap-title "archive"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           ;; :makeindex t
           )
          ;; Define any other projects here...
          ("blog-static"
           :base-directory "~/projects/website/org"
           :base-extension "scss\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mp4"
           :publishing-directory "~/projects/website/public/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("blog" :components ("blog-notes" "blog-static"))))

  )

(use-package org-journal
  :ensure t
  :after org
  :defer t
  :bind (("C-c C-j" . org-journal-new-entry))
  :custom
  (org-journal-dir "~/Dropbox/Org/journal/")
  :config
  (defun org-journal-save-entry-and-exit ()
    "Convenience! Save _and_ close buffer (like org-capture). Idea from org-journal github page"
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit))

;;; --- web based Stuff
(use-package web-mode
  :ensure t
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package rainbow-mode
  :ensure t)
(use-package js2-mode
  :ensure t)
;; update browser without reloading after editing
(use-package impatient-mode
  :ensure t)
(use-package emmet-mode
  :ensure t
  :hook ((scss-mode-hook . emmet-mode)
         (css-mode-hook . emmet-mode)
         (sgml-mode-hook . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t))
(use-package scss-mode
  :ensure t)

(use-package simple-httpd ; easy httpd local Server
  :ensure t
  :functions httpd-send-header
  :config
  (defservlet uptime "text/plain" ()
    (princ (emacs-uptime)))
  (defun httpd-here ()
    (interactive)
    (setf httpd-root default-directory))
  (defadvice httpd-start (after httpd-query-on-exit-flag activate)
    (let ((httpd-process (get-process "httpd")))
      (when httpd-process
        (set-process-query-on-exit-flag httpd-process nil)))))

;;; --- Prose and related
;; spell checking, definitely needed
(use-package flyspell
  :disabled
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config
  ;; Need to give some default usages and directories
  (setq ispell-program-name "/usr/bin/aspell")
  (setq ispell-dictionary "en_US")
  (global-font-lock-mode t) ; Skeptical of this. Color instead of underline
  (custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
  (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws"))

;; Grammar and style
(use-package langtool
  :disabled
  :ensure t
  :custom
  (langtool-language-tool-jar "C:/Users/LiamP/Applications/LanguageTool-4.8/languagetool-commandline.jar")
  :config
  (add-hook 'langtool-error-exists-hook
            (lambda ()
              (langtool-correct-buffer)
              (langtool-check-done)))

  (global-set-key (kbd "C-c ^")
                  (lambda ()
                    (interactive)
                    (message "Checking grammar...")
                    (langtool-check))))

;; TODO => writegood mode?
;;; --- LaTeX
(use-package magic-latex-buffer
  :ensure t
  :hook latex-mode
  :custom
  (magic-latex-enable-block-highlight nil)
  (magic-latex-enable-suscript        t)
  (magic-latex-enable-pretty-symbols  t))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . flyspell-buffer)
         (LaTeX-mode . turn-on-reftex))
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (TeX-save-query nil)
  :config
  (setq-default preview-scale-function 3.0
                TeX-master nil
                TeX-command-extra-options "--shell-escape"))

(use-package tex-fold
  :ensure auctex
  :hook (TeX-mode-hook . TeX-fold-mode))

;; The LaTeX macros available through this mechanism are fully configurable -
;; see the variable `cdlatex-math-symbol-alist'.
(use-package cdlatex
  :ensure t
  :hook ((org-mode . turn-on-org-cdlatex)
         (LaTeX-mode-hook . cdlatex-mode)))


;;; --- PDF viewing and all that
(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-resize-factor 1.05)
  :config
  (setq-default pdf-view-display-size 'fit-page))

;;; --- Super miscelleneous Stuff
;; Browsing the internet in emacs
(use-package eww
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (defun toggle-browse-fcn ()
    "Toggle between link browsing functions for convenience"
    (interactive)
    (if (equal browse-url-browser-function 'eww-browse-url)
        (setq browse-url-browser-function 'browse-url-chromium)
      (setq browse-url-browser-function 'eww-browse-url)))
  (global-set-key (kbd "<f9>") #'toggle-browse-fcn))

;; Food recipes in org mode!! TODO => Figure out how this works, make something
;; to scrape for ingredient prices and all that from:
;; - [ ] BJs
;; - [ ] Harris Teeters
;; - [ ] Hmart ****
;; - [ ] Lotte
;; - [ ] whole foods
;; - [ ] Giant
(use-package org-chef
  :disabled
  :ensure t)

;;;;;;;;;;;;;;;;;;;; Anki
(use-package anki-editor
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)

  ;; Allow Emacs to access content from clipboard.
  (setq x-select-enable-clipboard t
        x-select-enable-primary t))

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable))


;;;;;;;;;;;;;;;;;;;; shells
(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

(use-package eshell
  :ensure t
  :custom
  (shell-file-name "bash")
  ;; (shell-command-switch "-ic")
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t)
  (eshell-prefer-lisp-functions nil)
  (eshell-destroy-buffer-when-process-dies t)
  :init
  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "e" "find-file $1")
                                (eshell/alias "ff" "find-file $1")
                                (eshell/alias "emacs" "find-file $1")
                                (eshell/alias "ee" "find-file-other-window $1")

                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")
                                (eshell/alias "d" "dired $1")
                                (eshell/alias "ll" "ls -AlohG --color=always")))

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
  (setq eshell-highlight-prompt nil)

  ;; from abrochard
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize "┌─[" 'face `(:foreground "green"))
           (propertize (user-login-name) 'face `(:foreground "red"))
           (propertize "@" 'face `(:foreground "green"))
           (propertize (system-name) 'face `(:foreground "lightblue"))
           (propertize "]──[" 'face `(:foreground "green"))
           (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
           (propertize "]──[" 'face `(:foreground "green"))
           (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
           (propertize "]\n" 'face `(:foreground "green"))
           (propertize "└─>" 'face `(:foreground "green"))
           (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green")))))

  (setq eshell-visual-commands '("htop" "vi" "screen" "top" "less"
                                 "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"
                                 "vim"))

  (setq eshell-visual-subcommands '("git" "log" "diff" "show" "ssh"))
  (setenv "PAGER" "cat")

  (defun eshell/clear ()
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo)) ;; The echo command suppresses output

  (defun eshell/close ()
    (delete-window))

  (defun eshell-pop--kill-and-delete-window ()
    (unless (one-window-p)
      (delete-window)))

  (add-hook 'eshell-exit-hook 'eshell-pop--kill-and-delete-window)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-M-a") 'eshell-previous-prompt)
              (define-key eshell-mode-map (kbd "C-M-e") 'eshell-next-prompt)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))

  ;; from http://www.howardism.org/Technical/Emacs/eshell-present.html
  (defun eshell/-buffer-as-args (buffer separator command)
    "Takes the contents of BUFFER, and splits it on SEPARATOR, and
runs the COMMAND with the contents as arguments. Use an argument
`%' to substitute the contents at a particular point, otherwise,
they are appended."
    (let* ((lines (with-current-buffer buffer
                    (split-string
                     (buffer-substring-no-properties (point-min) (point-max))
                     separator)))
           (subcmd (if (-contains? command "%")
                       (-flatten (-replace "%" lines command))
                     (-concat command lines)))
           (cmd-str  (string-join subcmd " ")))
      (message cmd-str)
      (eshell-command-result cmd-str)))

  (defun eshell/bargs (buffer &rest command)
    "Passes the lines from BUFFER as arguments to COMMAND."
    (eshell/-buffer-as-args buffer "\n" command))

  (defun eshell/sargs (buffer &rest command)
    "Passes the words from BUFFER as arguments to COMMAND."
    (eshell/-buffer-as-args buffer nil command)))

;; (load-file "c:/msys64/home/LiamP/git_garbage/agda/.stack-work/install/c0474cbe/share/x86_64-windows-ghc-8.8.3/Agda-2.6.1/emacs-mode/agda2.el")

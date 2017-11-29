(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; paren mode
(setq show-paren-mode t)

;;; Disable window chrome
(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))



;;; Load current favorite theme, -insert theme-

;; (require 'powerline)
;; (require 'moe-theme)
;; (powerline-moe-theme)
;; ;(setq moe-theme-highlight-buffer-id t)
;; (moe-theme-set-color 'green)


;;; im sick of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Fancy lambdas
(global-prettify-symbols-mode t)

;;; screw the bell
(setq ring-bell-function 'ignore)

;;; Scroll conservatively
(setq scroll-conservatively 100)

;;; Font functionality


(setq lp/default-font "Consolas")

(setq lp/default-font-size 14)
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


;;; global-hl-line-mode softly highlights bg color of line. Its nice.

(when window-system
  (global-hl-line-mode))

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

;;;  Use =diff-hl= package to highlight changed-and-uncommitted lines
(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;;; I'd prefer that compilation output goes to *compilation buffer*
;;; Rarely have the window selected, so the output disappears past the
;;; bottom of the window
(setq compilation-scroll-output t)

;;; A quick hook to C modes to quickswap to =.h= files or =.c= files. It's nice
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;; quicktramp setup
(setq tramp-default-method "ssh")

;;; We want all lispy languages to use =paredit-mode= and =rainbow-delimiters
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

;;; When using elsip, =eldoc-mode= is quite nice for documentation
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; God bless magit and all that it does
(require 'magit)
;; bring up status with "C-x g"
(global-set-key (kbd "C-x g") 'magit-status)

;; Confirming before pushing every time is slightly inconvenient, so I turn that off
(setq magit-push-always-verify nil)


;;; Projectile !
(require 'projectile)
;; The default binding of projectile-ag is too clunky for me. I prefer
;; C-c C-v for this and C-c v for the original.
(defun lp/search-project-for-symbol-at-point ()
  "Use projectile-ag to search current project for the current symbol."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'lp/search-project-for-symbol-at-point)

;;; Currently using eshell for my shell sessions. Bound to C-c s.
(global-set-key (kbd "C-c s") 'eshell)



;;; Org-mode stuff, publishing / task management
(require 'org)
(require 'org-bullets)
;; Pretty bullets are better than list of asterisks :D
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

;; Also use a down arrow instead of ellipsis
(setq org-ellipsis "⤵")

;; When editing code snippet/ block, use syntax highlighting for that language
;; Also don't open new window for src blocks
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)

;; Org-capture management + Tasks
(setq org-directory "~/Dropbox/org/")

(defun org-file-path (filename)
  "Return absolute address of an org file give its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))


;; I keep all of my todos in =~/org/index.org= so I derive my agenda from there
(setq org-agenda-files (list org-index-file))

;; Bind C-c C-x C-s to mark todo as done and archive it
(defun lp/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it"
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))
(define-key org-mode-map (kbd "C-c C-x C-s") 'lp/mark-done-and-archive)
(setq org-log-done 'time) ; also record when the TODO was archived


;;; Lets get some ORGCAPTURETEMPLATES :O
;; Keep TODOs in ~/org/index.org
;; Keep proj ideas in ~/org/proj-ideas.org
;; Keep running groceries/buy list in ~/org/groceries.org
;; Keep running list of music to check out in ~/org/music.org
;; Cool sites to check out in ~/org/websites.org
;; others TBD
(setq org-capture-templates
      '(("w" "Wishlist"
         checkitem
         (file "~/Dropbox/org/wishlist.org"))
        ("g" "Groceries"
         checkitem
         (file "~/Dropbox/org/groceries.org"))
        ("r" "Reading"
         checkitem
         (file "~/Dropbox/org/to-read.org"))
        ("m" "Music"
         entry
         (file "~/Dropbox/org/music.org"))        
        ("s" "sites"
         entry
         (file "~/Dropbox/org/websites.org")
         "* %?\n")
        ("t" "Todo"
         entry
         (file+headline org-index-file "Tasks")
         "* TODO %?\n")
        ("j" "Journal"
         entry
         (file "~/Dropbox/org/journal.org")
         "** %u :journal: \n %?")))

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
;;; TODO add export settings for org-mode

;;;;
;; Useful functions
;;;;
(defun lp/kill-current-buffer ()
  "Just kill the buffer man (no prompt when killing buffer"
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



;;;;
;; Some general settings
;;;;

;;; Always killcurrent buffer
(global-set-key (kbd "C-x k") 'lp/kill-current-buffer)


;;; Look for executables in bin
(setq exec-path (append exec-path '("/user/local/bin")))


;;; Always use spaces for indentation
;; fuck tabs
(setq-default indent-tabs-mode nil)


;;; Yasnippet configuration
(yas-global-mode 1)
;; Don't want =ido= to auto indent snippet inserts
(setq yas/indent-line nil)


;;; Ido configuration
(require 'flx-ido)
(require 'ido-completing-read+)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;;; Use smex for M-x + ido
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;;; Auto wrap paragraphs in some modes (auto-fill-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; sometimes i don't want to wrap text though, so we will toggle with C-c q
(global-set-key (kbd "C-c q") 'auto-fill-mode)

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


;;; Projectile everywhere obviously
(projectile-global-mode)

;;; Lets bind C-c C-k to compile buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;;;;
;; Some custom keybinds
;;;;

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)

;; Gotta keep those buffers clean
(global-set-key (kbd "C-c n") 'lp/cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'lp/cleanup-buffer)

;; Open up a randomly generated scratch buffer (cause i like scratch buffers)
(global-set-key (kbd "<f12>") 'lp/generate-scratch-buffer)

;; Compile on a keybind
(global-set-key (kbd "C-<f7>") 'compile)

;; Quickly comment region
(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-region)


;;;;
;; TEX!
;;;;

(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(set-default 'preview-scale-function 2.0)
;(require 'flymake)



;; (defun flymake-(get symbol {2:propname})-tex-args (file-name)
;;        (list "pdflatex"
;;              (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;(add-hook 'LaTeX-mode-hook 'flymake-mode)




;;;; TODO - Rest of "My custom settings" in configuration.org

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "c03d60937e814932cd707a487676875457e0b564a615c1edfd453f23b06fe879" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-tags-column 80)
 '(package-selected-packages
   (quote
    (powerline moe-theme f auctex ido-vertical-mode flx-ido ido-ubiquitous yasnippet org-bullets rainbow-delimiters projectile paredit org-link-minor-mode magit diminish diff-hl)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

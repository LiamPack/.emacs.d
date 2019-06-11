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

                                        ; general setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(setq gc-cons-threshold 100000000)
;; chicken before the
(eval-when-compile
  (require 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'unannoy)
(require 'extras)
(require 'utility)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;; Time to load everything from the lisp/ directory
;; (setq active-directory-files (list "~/.emacs.d/lisp/"))
;; (defun active-config-directory ()
;;   "Where active package configurations are kept."
;;   (format "%slisp/" user-emacs-directory))

;; (defun load-use-file (name)
;;   "Load a use file NAME expect an error if it doesn't map to an existing file."
;;   (let (file)
;;     (setq file (concat (active-config-directory) name))
;;     (unless (or (equal name ".") (equal name ".."))
;;       (message "Using config: %s" file)
;;       (if (file-exists-p file)
;;           (load-file file)
;;         (message "Warning: %s doesn't exist" file)))))

;; (dolist (use-file
;;          (directory-files (active-config-directory)))
;;   (load-use-file use-file))

;; (put 'narrow-to-region 'disabled nil)

;;(global-set-key (kbd "C-?") 'help-command)

;;;; Setup functions put towards the forces of good

                                        ; useful functions + keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14 ")

(defun lp/generate-scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (emacs-lisp-mode))

;; Allow newly created scratch buffers to be elisp mode
(add-to-list 'auto-mode-alist '("^scratch-.*$" . emacs-lisp-mode))

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

;; TODO - find a use for this
(defun lp/org-open-point ()
  "Open org mode heading in another window, expand it, and narrow it"
  (interactive)
  (org-beginning-of-line)
  (setq goal-point (point))
  (call-interactively #'clone-indirect-buffer-other-window)
  (while (not (= goal-point (point)))
    (goto-char goal-point)
    (org-beginning-of-line)
    (org-cycle)
    (goto-char goal-point)
    (org-beginning-of-line))
  (call-interactively #'org-next-visible-heading)
  (narrow-to-region goal-point (point))
  (goto-char goal-point)
  (fset 'tab
        (lambda (&optional arg) "Keyboard macro." (interactive "p")
          (kmacro-exec-ring-item (quote ([tab] 0 "%d")) arg)))
  (tab)) ;; It basically just narrows right where you are.

(global-set-key (kbd "C-c o") 'lp/org-open-point)


;; Always killcurrent buffer
(global-set-key (kbd "C-x k") '(lambda () (interactive) (kill-buffer (current-buffer))))

;; Look for executables in bin
(setq exec-path (append exec-path '("/user/local/bin")))

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-TAB") 'hippie-expand)
(global-set-key (kbd "C-j") 'join-line) ; note that paredit binds this to (paredit-newline)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c n") 'lp/cleanup-buffer)
(global-set-key (kbd "<f12>") 'lp/generate-scratch-buffer)
(global-set-key (kbd "C-c C-k") #'eval-buffer)
(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "<f5>")  #'revert-buffer)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-@") 'align-regexp)
(global-set-key (kbd "C-c e") 'eval-and-replace) ; this one is pretty cool.
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)
;; When popping the mark, continue popping until the cursor actually
;; moves Also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; backspace change!
;;(global-set-key (kbd "C-h") 'delete-backward-char)?
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-m") 'newline-and-indent)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(use-package which-key
  :ensure t
  :config (which-key-mode 1))


;;;;;;;;;;;;;;;;;;;; File Management
;; clean up permissions and owners, less noisy
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            (require 'dired-x)
            (dired-dotfiles-toggle)))

;; disable ls by default
(setq dired-use-ls-dired nil)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :diminish recentf-mode
  :config
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

;; Crazy useful but I don't know how to use it.
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

;; Not-shitty file tree popup
(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 30
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t)
  (global-set-key (kbd "C-c f t") 'neotree-toggle))

;; handle very large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;;;;;;;;;;;;;;;;;;;; Writing Configuration

;;; Flycheck stuff
(use-package flycheck
  :ensure t
  :defer t
  :config
  ;; Turn flycheck on everywhere
  ;; (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil)))

;; Highlights bad word choices apparetnly
(use-package writegood-mode
  :ensure t
  :disabled t
  :defer t
  :hook text-mode
  :diminish writegood-mode
  :bind (("C-c g" . writegood-mode)
         ("\C-c\C-gg" . writegood-grade-level)
         ("\C-c\C-ge" . writegood-reading-ease))
  :config
  (add-to-list 'writegood-weasel-words "actionable"))



;; Clears up Emacs to only be the frame you're looking at
(use-package writeroom-mode ; TODO make a bind for this mode in org-mode
  :ensure t
  :after visual-fill-column
  :diminish writeroom-mode)
(use-package visual-fill-column
  :ensure t)

;;;;;;;;;;;;;;;;;;;; Org

;;;;;;;;;;;;;;;;;;;; Programming languages

;;;; C
;;; A quick hook to C modes to quickswap to =.h= files or =.c= files. It's nice
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
;; also gdb is cool
(setq gdb-many-windows 't)

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


;;;; Lisps
;; Make some functions to call to bring up the ELISP repl or to
;; evaluate and run smoe code
(defun ert-all ()
  (interactive)
  (ert t))

(defun ielm-repl ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(define-key emacs-lisp-mode-map (kbd "C-x r")   #'ert-all)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer*)
(defalias 'lisp-interaction-mode 'emacs-lisp-mode)

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(\\(\\(?:\\(?:\\sw\\|\\s_\\)+-\\)?"
             "def\\(?:\\sw\\|\\s_\\)*\\)\\_>"
             "\\s-*'?" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
    (1 'font-lock-keyword-face)
    (2 'font-lock-function-name-face nil t)))
 :low-priority)

;; slime for our clisp goodness
(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-repl))
  (setq inferior-lisp-program "/usr/bin/sbcl") ; if it exists!
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

;; paren stuff
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

;;;; R / ess
;;; Statistics and R in emacs. Need to have R installed obviously.
(use-package ess
  :ensure t
  :config
  (setq-default inferior-S+6-program-name "Splus")
  (setq-default inferior-R-program-name "R"))


;;;; Python
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
  (setq python-indent-offset 2)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package company-jedi
  :ensure t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;;;; Haskell
(use-package haskell-mode               ; Haskell major mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)
              ;; Recommended Haskell Mode bindings, see
              ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
              )
  :config
  (setq haskell-tags-on-save t ; Regenerate TAGS on save
        haskell-process-log t  ; Show log for GHCI process
        ;; Remove unused imports and auto-import modules
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package haskell                    ; Interactive Haskell
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-`" . haskell-interactive-bring)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c c" . haskell-process-cabal)
              :map interactive-haskell-mode-map
              ("C-c m t" . haskell-mode-show-type-at))
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-compile            ; Haskell compilation
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>" . haskell-compile))
  :config
  ;; Build with Stack
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode                 ; Cabal files
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-cabal-mode-map
              ("C-`" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)))

(use-package hindent                    ; Haskell indentation
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (setq hindent-style "gibiansky"))

;;;; Web-mode ( javascript httpd ... )
(use-package web-mode
  :ensure t
  :diminish web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.jsp\\'"        . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.html?\\'"      . web-mode)
         ("\\.hbs\\'"        . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (define-key web-mode-map (kbd "<backtab>") 'web-mode-fold-or-unfold))

(use-package emmet-mode
  :ensure t
  :mode ("\\.html" . emmet-mode)
  :diminish emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (setq emmet-move-cursor-between-quotes t))

(use-package impatient-mode ; auto-update browser without having to
                                        ; reload when editing web stuff
  :defer t
  :ensure t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(use-package simple-httpd ; httpd stuff
  :ensure t
  :defer t
  :functions httpd-send-header
  :config
  (progn
    (defservlet uptime "text/plain" ()
      (princ (emacs-uptime)))
    (defun httpd-here ()
      (interactive)
      (setf httpd-root default-directory))
    (defadvice httpd-start (after httpd-query-on-exit-flag activate)
      (let ((httpd-process (get-process "httpd")))
        (when httpd-process
          (set-process-query-on-exit-flag httpd-process nil))))))

(use-package js2-mode ; javascript editing
  :ensure t
  :diminish (js-mode . "js")
  :mode "\\.js$"
  :config
  (progn
    (use-package js2-refactor
      :ensure t)
    (use-package xref-js2
      :ensure t)

    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook (lambda () (progn
                                          (setq mode-name "js2")
                                          (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


    (setf js2-skip-preprocessor-directives t)
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "skewer"
                    "console" "phantom"))))

;; more info here https://github.com/skeeto/skewer-mode
;; check it out https://github.com/smihica/emmet-mode
;; C-x C-e: Evaluate the form before the point and display the result in the minibuffer. If given a prefix argument, insert the result into the current buffer.
;; C-M-x: Evaluate the top-level form around the point.
;; C-c C-k: Load the current buffer.
;; C-c C-z: Select the REPL buffer.
(use-package skewer-mode
  :ensure t
  :diminish (skewer-mode . "sk")
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package restclient ; for some HTTP shenanigens!
  :ensure t)


;;;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; LaTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package magic-latex-buffer
  :ensure t
  :config
  (add-hook 'latex-mode-hook 'magic-latex-buffer)
  ;; (setq magic-latex-enable-block-highlight nil
  ;;       magic-latex-enable-suscript        t
  ;;       magic-latex-enable-pretty-symbols  t)
  )

(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-PDF-mode t)
  ;; The damn text is too tiny on the preview.
  (set-default 'preview-scale-function 3.0)

  ;; revert pdf-view after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  ;; (setq TeX-view-program-list
  ;;       '(("Evince" "evince --page-index=%(outpage) %o")))
  ;; (setq TeX-view-program-selection '((output-pdf "Evince")))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; TODO Alt config!
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-save-query nil)
  (setq-default TeX-command-extra-options "--shell-escape")
  (setq TeX-error-overview-open-after-TeX-run t)
  (setq TeX-electric-math '("$" . "$"))
  (setq TeX-electric-sub-and-superscript t)
  ;; fix for company completion
  ;; (define-key TeX-mode-map (kbd "TAB") 'tab-indent-or-complete)
  ;; (define-key TeX-mode-map [tab] 'tab-indent-or-complete)

  ;; Fix auto-fill in math mode
  (setq-default LaTeX-fill-break-at-separators (quote (\\\( \\\[ \\\])))

  ;; spelling corrections
  (require 'ispell)
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)

  (setq ispell-dictionary-base-alist
        '(("en_US"
           "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
           ("-d" "en_US" "-i" "iso-8859-1") nil iso-8859-1)
          ("en_GB"
           "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
           ("-d" "en_GB" "-i" "iso-8859-1") nil iso-8859-1)
          ("de_DE"
           "[a-zäöüßA-ZÄÖÜ]" "[^a-zäöüßA-ZÄÖÜ]" "[']" nil
           ("-d" "de_DE" "-i" "iso-8859-1") nil iso-8859-1)))
  (eval-after-load "ispell"
    (progn
      (setq ispell-dictionary "en_US")
      (setq ispell-silently-savep t))) ; save personal dict without confirmation
  )

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (setq reftex-plug-into-AUCTeX t
        ;; Automatically derive labels, and prompt for confirmation
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(
          ;; Additional label definitions for RefTeX.
          ("definition" ?d "def:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("definition" "def.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("theorem" "th.") -3)
          ("example" ?x "ex:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("example" "ex") -3)
          ;; Algorithms package
          ("algorithm" ?a "alg:" "~\\ref{%s}"
           "\\\\caption[[{]" ("algorithm" "alg") -3)))

  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}"))))
    (setq reftex-cite-format 'biblatex))
  :diminish reftex-mode)

(use-package cdlatex
  :disabled
  :ensure t
  :init (add-hook 'LaTeX-mode-hook #'cdlatex-mode))


;;;; Language Server Protocol (lsp)




;;;;;;;;;;;;;;;;;;;; Aesthetics
;;; General aesthetic configurations for emacs
;;; Highlight TODO keywords and all that kinda stuff on prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face prepend)))))

;;
;; espresso ; cyberpunk ; moe-light ;
;;* good themes
;;** base16
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
;;    * classic-{dark,light}
;; * avk-daylight
;;
;;* actual good themes
;;  * leuven
;;  * porple
;;  * doom-tomorrow-night
;;    * Any of the doom ones really
;;  * Habamax Theme - a little plain
;;  * Also hydanatantantatna-theme
;;  * gruvbox
;;  * tsdh-light
;;  * tron theme https://github.com/ianpan870102/Emacs-Tron-Legacy-Theme
(require 'use-package)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/modern-themes")

;; sick of this blinking
(blink-cursor-mode -1)

;; stop truncating lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; also fonts
(set-locale-environment "UTF-8")

;; powerline theme where the modes are on the right side wow
(use-package powerline
  :disabled
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

;; Yet another mode-line package to clean things up
;; The one that looks the least bad imo.
(use-package minions
  :ensure t
  :config
  (minions-mode))

;; (use-package moody
;;   :ensure t
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))


(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))


;;(load-theme 'kaolin-eclipse)
;;(load-theme 'tsdh-light)
;;(load-theme 'manoj-dark t)
;; (set-face-attribute 'mode-line nil :background "NavajoWhite")
;; (set-face-attribute 'mode-line-inactive nil :background "#FAFAFA")

(use-package moe-theme
  :ensure t
  :disabled t
  :config
  (setq moe-light-pure-white-background-in-terminal t)
  (moe-theme-set-color 'purple)
  ;; Resize titles
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
  (require 'moe-theme-switcher)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  (setq moe-theme-highlight-buffer-id t)
  (moe-light)
  )

(use-package color-theme-modern
  :ensure t
  :config
  (load-theme 'midnight)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box       nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; screw the bell
(setq ring-bell-function 'ignore)

;; Scroll conservatively
(setq scroll-conservatively 100)

;; time on modeline is cool
(use-package time                       ; Show current time
  :ensure t
  :bind (("C-c w t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)")))
  (setf display-time-default-load-average nil
        display-time-use-mail-icon t
        display-time-24hr-format t)
  (display-time-mode))


;; iosevka, consolas, source code pro, Fira Code, dejavu, IBM 3270,
;; Fantasque Sans Mono, Terminus, overpass mono
(setq lp/default-font "DejaVu Sans mono")

;;; Font functionality
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

;; global-hl-line-mode softly highlights bg color of line. Its nice.
(when window-system
  (global-hl-line-mode))

;; Helps with stupid ^L characters - allows a page break to appear! Nice for org stuff
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

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

  ;; Diminishes that I don't remember if I actually put in the usepackage
  ;; :diminish declaration
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

(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;;;; Comint
;;; REPL / terminal / minor process interactions
(require 'comint)

(define-key comint-mode-map (kbd "<down>") #'comint-next-input)
(define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-n") #'comint-next-input)
(define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
(setf comint-prompt-read-only t
      comint-history-isearch t)

;;;;;;;;;;;;;;;;;;;; Version Control

;;;; magit!
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

(use-package git-gutter ; TODO - git gutter keybinds, going to different hunks and staging only certain portions!
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))


;;;;;;;;;;;;;;;;;;;; Fancy Navigation
;;;; anzu
;;; Interactive searching and regexp replacing to help see what goes down
(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :defer 1
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  (global-anzu-mode)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(nvm-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => ")))


;;;; Counsel / Ivy / Swiper
(use-package counsel                    ; Ivy-powered commands
  :ensure t
  :diminish counsel-mode
  :init (counsel-mode)
  :bind (([remap execute-extended-command]  . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ;;("C-c i 8"                        . counsel-unicode-char)
         ("C-c f a"                        . counsel-ag)
         ("C-c f m"                        . counsel-imenu))
  :config

  (progn
    (unbind-key "C-x p" counsel-mode-map)
    (global-set-key (kbd "C-x p") 'pop-to-mark-command)))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init  (with-eval-after-load 'ido
           (ido-mode -1)
           ;; Enable ivy
           (ivy-mode 1))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d "))


;;;; iedit
;;; For editing multiple things at the same time
(use-package iedit
  :ensure t
  :bind (("C-:" . #'iedit-mode)))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree                  ; Branching undo
  :disabled t
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package expand-region
  :ensure t
  :bind ("C-," . er/expand-region))


;;;;;;;;;; Window Management (kinda fancy navigation)
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
;; Standard window commands
(bind-key "C-c w =" #'balance-windows)
(bind-key "C-c w k" #'delete-window)
(bind-key "C-c w m" #'delete-other-windows)


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

;; Configure `display-buffer' behaviour for some special buffers.
(setq
 display-buffer-alist
 `(
   ;; Put REPLs and error lists into the bottom side window
   (,(rx bos
         (or "*Help"                         ; Help buffers
             "*Warnings*"                    ; Emacs warnings
             "*Compile-Log*"                 ; Emacs byte compiler log
             "*compilation"                  ; Compilation buffers
             "*Flycheck errors*"             ; Flycheck error list
             "*shell"                        ; Shell window
             "*sbt"                          ; SBT REPL and compilation buffer
             "*ensime-update*"               ; Server update from Ensime
             "*SQL"                          ; SQL REPL
             "*Cargo"                        ; Cargo process buffers
             (and (1+ nonl) " output*")      ; AUCTeX command output
             ))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . bottom)
    (reusable-frames . visible)
    (window-height   . 0.33))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package ibuffer                    ; Better buffer list
  :ensure t
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq
   ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 16 :left)
           " "
           filename-and-process)
     (mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :disabled t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))


;;; experimental TODO
(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun lunaryorn-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . lunaryorn-toggle-golden-ratio))
  :config
  (setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   ;; Exclude a couple of special modes from golden ratio, namely
   ;; Flycheck's error list, calc
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                dired-mode
                                ediff-mode
                                )
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

;;;;;;;;;;;;;;;;;;;; Company
(use-package company
  :ensure t
  :config
  (setq auto-revert-check-vc-info 'nil)
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends)))

(use-package company-quickhelp          ; Show help in tooltip
  ;;:disabled t                           ; M-h clashes with mark-paragraph
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package helm-company               ; Helm frontend for company
  :disabled t
  :ensure t
  :after company
  :defer t
  :bind (:map company-mode-map
              ([remap complete-symbol] . helm-company)
              ([remap completion-at-point] . helm-company)
              :map company-active-map
              ("C-:" . helm-company)))


;;;;;;;;;;;;;;;;;;;; Elfeed
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


;;;;;;;;;;;;;;;;;;;; eshell
;; check out
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
   eshell-destroy-buffer-when-process-dies t)
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

  (bind-key "C-!" 'eshell-here))

(setq eshell-highlight-prompt nil)
(setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

;; (defun eshell-here ()
;;   "Opens up a new shell in the directory associated with the
;;     current buffer's file. The eshell is renamed to match that
;;     directory to make multiple eshell windows easier."
;;   (interactive)
;;   (let* ((height (/ (window-total-height) 3)))
;;     (split-window-vertically (- height))
;;     (other-window 1)
;;     (eshell "new")
;;     (insert (concat "ls"))
;;     (eshell-send-input)))

;; (bind-key "C-!" 'eshell-here)

;;;;;;;;;;;;;;;;;;;; Misc Packages

;;;; emacs calc
(use-package calc
  :ensure t
  :bind ("C-c =" . calc)
  :config (setf calc-display-trail nil))

;;;; ediff
;;; diff, but in emacs!
(use-package ediff
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-diff-options "-w")
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (derived-mode-p 'outline-mode)
                (outline-show-all)))))

;;;; erc
;;; IRC client right in emacs. lmao.
;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
;; shamelessly taken from this thread
(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "#bash" "#bitcoin"
                                  "#emacs" "#gentoo" "#i3" "#latex" "#org-mode" "#python")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
  (erc-lurker-threshold-time 43200)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  ;; TODO set auth sources, some other helper functions to do things..
  )


(use-package erc-hl-nicks ; coloring pseudonyms
  :after erc)

;;;;;;;;;;;;;;;;;;;; Org
(use-package org-bullets
  :ensure t
  :config
  (setq org-ellipsis "⤵"))

(use-package poporg ; pop-out org mode window to edit comments. opposite of the embedding of source blocks
  :bind (("C-c /" . poporg-dwim)))

(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
         ("\C-cb" . org-iswitchb))
  :config
  (require 'org-habit)
  (unbind-key "C-," org-mode-map)       ;expand-region
  (unbind-key "C-'" org-mode-map)       ;avy

  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode)) )

  ;; Some latex stuff in org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq-default org-highlight-latex-and-related '(latex script entities))
  (setq org-latex-listings 'minted)

  ;; Evaluates latex fragments behind the $ after pressing $, <SPC>
  (defun krofna-hack ()
    (when (looking-back (rx "$ "))
      (save-excursion
        (backward-char 1)
        (org-toggle-latex-fragment))))

  (add-hook 'org-mode-hook
            (lambda ()
              (org-cdlatex-mode)
              (add-hook 'post-self-insert-hook #'krofna-hack 'append 'local)))

  ;; Some nice latex pretty-entites!
  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-use-speed-commands t)


  ;; More latex classes on export
  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("IEEEtran"
                 "\\documentclass{IEEEtran}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}"))) ;; NOTE: If this isn't working, make sure to delete /
  ;; byte-recompile the /elpa/org/.. directory!
  ;; enable language compiles
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (python . t)
     ;;     (sh . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (ipython . t)
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
                       ":ml:" ":sites:" ":games:" ":music:" ":math:" ":podcasts:" ":videos:" ":papers:" ":movies:"))

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
           (file "~/Dropbox/Org/groceries.org")
           "- [ ] %?\n")
          ("i" "Ideas"
           entry
           (file+headline "~/Dropbox/Org/ideas.org" "Project Ideas")
           "** [#%^{9}] %?\n")
          ("j" "Journal"
           entry
           (file+datetree "~/Dropbox/Org/journal.org")
           "** %U :journal:\n%?\n good things that happened today?\n")
          ("t" "to-read"
           entry
           (file+headline "~/Dropbox/Org/to-read.org" "inbox")
           "** TODO %^{to-read}  %^g\n %U")
          ("z" "Todo"
           entry
           (file+headline org-index-file "Tasks")
           "* TODO %^{Task} %^G\n %U\n%?")
          ("p" "Personal todo"
           entry
           (file+headline org-personal-file "general")
           "* TODO %^{Task} %^g\n %?")
          ("a" "anki basic" entry (file+headline "~/Dropbox/Org/logs/added_anki.org" "Basic")
           "* all :deck: \n** Item :note: \n\t:PROPERTIES:\n\t:ANKI_DECK: all\n\t:ANKI_NOTE_TYPE: basic\n\t:ANKI_TAGS: %^{tags} \n\t:END:\n*** Front\n \n*** Back\n%?")))

;;; Org Keybindings
  ;; Useful keybinds
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)


  (define-key global-map (kbd "C-c k") (lambda () (interactive) (find-file "~/anki/all_anki.csv")))
  ;; (defun anki-hook ()
  ;;   (when (string= "a" (plist-get org-capture-plist :key))
  ;;     (anki-editor-push-notes)))

  ;; (add-hook 'org-capture-mode-hook #'ank-hook)

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
                                        ; clocking!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ok back to clocking
;;;;;;;;;;;;;;;;;;;;

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
;; (setq org-clock-history-length 23)
;; ;; Resume clocking task on clock-in if the clock is open
;; (setq org-clock-in-resume t)
;; ;; Change tasks to NEXT when clocking in
;; (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; ;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; ;; Save clock data and state changes and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)
;; ;; Clock out when moving task to a done state
;; (setq org-clock-out-when-done t)
;; ;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persist t)
;; ;; Do not prompt to resume an active clock
;; (setq org-clock-persist-query-resume nil)
;; ;; Enable auto clock resolution for finding open clocks
;; (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; ;; Include current clocking task in clock reports
;; (setq org-clock-report-include-clocking-task t)

;; ox-hugo because why not

;; random org stuff now
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(let* ((variable-tuple
        (cond ((x-list-fonts "DejaVu Sans") '(:font "DejaVu Sans"))
              ;; ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ;; ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ;; ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;;(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent                ((t (:inherit (org-hide fixed-pitch))))))

;;;;;;;;;;;;;;;;;;;; Research Books org-ref

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

  :ensure t
  :config
  (require 'doi-utils)
  (setq org-ref-notes-directory "~/Dropbox/res"
        org-ref-bibliography-notes "~/Dropbox/res/notes.org"
        org-ref-default-bibliography '("~/Dropbox/res/index.bib")
        org-ref-pdf-directory "~/Dropbox/res/lib/"))

(use-package helm-bibtex

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


(use-package org-noter
  :ensure t)


;;;;;;;;;;;;;;;;;;;; Images and image manipulation
;; C-c + / C-c -: Zoom in/out image.

;; C-c M-m: Adjust image to current frame size.

;; C-c C-x C-s: Save current image.

;; C-c M-r / C-c M-l: Rotate image.

;; C-c M-o: Show image image+ have not modified.
(use-package image+
  :ensure t
  :defer t
  :disabled t
                                        ;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode)
               (imagex-auto-adjust-mode 1)
               (defhydra imagex-sticky-binding (global-map "C-x C-l")
                 "Manipulating Image"
                 ("+" imagex-sticky-zoom-in "zoom in")
                 ("-" imagex-sticky-zoom-out "zoom out")
                 ("M" imagex-sticky-maximize "maximize")
                 ("O" imagex-sticky-restore-original "restore original")
                 ("S" imagex-sticky-save-image "save file")
                 ("r" imagex-sticky-rotate-right "rotate right")
                 ("l" imagex-sticky-rotate-left "rotate left"))))
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.05)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)
  (setq-default pdf-view-display-size 'fit-page)

  ;; (global-unset-key (kbd "<double-down-mouse-4>"))
  ;; (global-unset-key (kbd "<double-down-mouse-5>"))
  ;; (global-unset-key (kbd "<double-down-mouse-6>"))
  ;; (global-unset-key (kbd "<double-down-mouse-7>"))
  ;; (global-unset-key (kbd "<down-mouse-4>"))
  ;; (global-unset-key (kbd "<down-mouse-5>"))
  ;; (global-unset-key (kbd "<down-mouse-6>"))
  ;; (global-unset-key (kbd "<down-mouse-7>"))
  ;; (global-unset-key (kbd "<mouse-4>"))
  ;; (global-unset-key (kbd "<mouse-5>"))
  ;; (global-unset-key (kbd "<mouse-6>"))
  ;; (global-unset-key (kbd "<mouse-7>"))

  (define-key pdf-view-mode-map (kbd "<double-mouse-7>") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "<double-mouse-6>") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "C-S-n") (lambda ()  (interactive) (pdf-view-next-line-or-next-page 3)))
  (define-key pdf-view-mode-map (kbd "C-S-p") (lambda ()  (interactive) (pdf-view-previous-line-or-previous-page 3)))
  ;; (defun up-one () (interactive) (scroll-up 1))
  ;; (defun down-one () (interactive) (scroll-down 1))
  ;; (defun left-one () (interactive) (scroll-left 1))
  ;; (defun right-one () (interactive) (scroll-right 1))
  ;; (global-set-key (kbd "<mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<down-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<down-mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<double-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<double-mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<double-down-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<double-down-mouse-5>") 'up-one)

  )

                                        ; slime and lisps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(require 'lisp-mode)
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

(use-package slime ; slime for our clisp goodness
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
;; (add-hook 'text-mode-hook 'hook-function)


(provide 'lisp-env)

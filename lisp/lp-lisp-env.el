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

;;(define-key emacs-lisp-mode-map (kbd "C-x r")   #'ert-all)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer*)
(defalias 'lisp-interaction-mode 'emacs-lisp-mode)

(use-package slime ; slime for our clisp goodness
  :straight t
  :config
  (slime-setup '(slime-repl))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; eldoc provides minibuffer hints for elisp things. it's super nice
(use-package eldoc
  :straight t
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

;; paren stuff
;; USING SMARTPARENS NOW!
;; (use-package paredit
;;   :straight t
;;   :diminish paredit-mode
;;  :hook ((emacs-lisp-mode-hook scheme-mode-hook lisp-mode-hook) . paredit-mode))

(use-package rainbow-delimiters
  :straight t
  :diminish rainbow-delimiters-mode
  :hook ((emacs-lisp-mode-hook scheme-mode-hook lisp-mode-hook prog-mode-hook) . rainbow-delimiters-mode))

(provide 'lp-lisp-env)

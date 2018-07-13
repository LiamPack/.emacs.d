;; iedit config file

(require 'use-package)

(use-package iedit
  :ensure t
  :config
  (defun ap/iedit-or-flyspell ()
    "Call `iedit-mode' or correct misspelling with flyspell, depending..."
    (interactive)
    (if (or iedit-mode
            (and (derived-mode-p 'prog-mode)
                 (not (or (nth 4 (syntax-ppss))
                          (nth 3 (syntax-ppss))))))
        ;; prog-mode is active and point is in a comment, string, or
        ;; already in iedit-mode
        (call-interactively #'ap/iedit-mode)
      ;; Not prog-mode or not in comment or string
      (if (not (equal flyspell-previous-command this-command))
          ;; FIXME: This mostly works, but if there are two words on the
          ;; same line that are misspelled, it doesn't work quite right
          ;; when correcting the earlier word after correcting the later
          ;; one

          ;; First correction; autocorrect
          (call-interactively 'flyspell-auto-correct-previous-word)
        ;; First correction was not wanted; use popup to choose
        (progn
          (save-excursion
            (undo)) ; This doesn't move point, which I think may be the problem.
          (flyspell-region (line-beginning-position) (line-end-position))
          (call-interactively 'flyspell-correct-previous-word-generic)))))

  (defun ap/iedit-mode (orig-fn)
    "Call `iedit-mode' with function-local scope by default, or global scope if called with a universal prefix."
    (interactive)
    (pcase current-prefix-arg
      ('nil (funcall orig-fn '(0)))
      ('(4) (funcall orig-fn))
      (_ (user-error "`ap/iedit-mode' called with prefix: %s" prefix))))

  ;; Override default `iedit-mode' function with advice.
  (advice-add #'iedit-mode :around #'ap/iedit-mode)
  
  (global-set-key (kbd "C-:") #'iedit-mode)
  )



(provide 'use-iedit)

(use-package icomplete-vertical
  :straight t
  :demand t
  :custom
;;  (completion-styles '(partial-completion substring intials flex))
  (completion-styles '(partial-completion substring initials flex))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-in-buffer nil)
  :config
  (icomplete-mode)
  ;;  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

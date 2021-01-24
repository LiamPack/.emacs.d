(require 'use-package)

(use-package icomplete-vertical
  :straight t
  :demand t
  :hook
  (icomplete-minibuffer-setup . visual-line-mode)
  :custom
  ;;  (completion-styles '(partial-completion substring intials flex))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-in-buffer t)
  ;; (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 5)
  (icomplete-vertical-prospects-height 7)
  :config
  (fido-mode -1)
  (icomplete-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("RET" . icomplete-force-complete-and-exit)
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-M-i" . minibuffer-complete)
              ("M-RET" . exit-minibuffer)))

(use-package minibuffer
  :demand
  :config
  (setq completion-styles '(partial-completion substring flex orderless))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.  Note that I actually do not use that buffer,
  ;; because I rely on Embark's version of it.
  ;;(setq completions-format 'one-column)
  (setq completions-detailed t)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  ;;(setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  :bind (("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         :map minibuffer-local-completion-map
         ("C-j" . exit-minibuffer)
         ("<tab>" . minibuffer-force-complete)
         ;; De facto deprecated as I use Embark and its own completions'
         ;; buffer.
         :map completion-list-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)))

(provide 'lp-icomplete)

(use-package prot-orderless
  :straight (:type built-in)
  :demand
  :config
  (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-literal
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-flex))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp)))

(use-package prot-minibuffer
  :straight (:type built-in)
  :demand
  :bind (("s-v" . prot-minibuffer-focus-mini-or-completions)
         :map completion-list-mode-map
         ("M-v" . prot-minibuffer-focus-mini)
         ("h" . prot-simple-describe-symbol) ; from `prot-simple.el'
         ;; Those are DE FACTO DEPRECATED generic actions for the
         ;; "*Completions*" buffer.  I normally use `embark' and its own
         ;; buffers.
         ("w" . prot-minibuffer-completions-kill-symbol-at-point)
         ("i" . prot-minibuffer-completions-insert-symbol-at-point)
         ("j" . prot-minibuffer-completions-insert-symbol-at-point-exit))
  :hook (minibuffer-setup-hook . prot-minibuffer-mini-cursor))


(use-package minibuffer
  :demand
  :after prot-minibuffer
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
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
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

(use-package orderless
  :straight t
  :after prot-orderless
  :demand
  :config
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher))
  ;;(setq completion-styles '(orderless))
  ;; SPC should never complete: use it for `orderless' groups.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil)))

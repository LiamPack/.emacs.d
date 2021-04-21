(require 'use-package)

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
  :bind (("M-V" . prot-minibuffer-focus-mini-or-completions)
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

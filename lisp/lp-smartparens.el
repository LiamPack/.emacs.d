(require 'use-package)

;; Yoinked from https://github.com/mrkkrp/dot-emacs/blob/9d2a3a77aa3e06cfd48ebefd53c984bf5cf5d914/mk/mk-packages.el
(use-package smartparens
  :straight t
  :demand
  :commands
  (sp-backward-kill-sexp
   sp-backward-sexp
   sp-kill-sexp
   sp-forward-sexp
   sp-select-next-thing
   sp-kill-hybrid-sexp
   sp-add-to-previous-sexp)
  :init
  (setq
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil)
  :config
  (smartparens-global-mode 1)
  (advice-add 'sp-add-to-previous-sexp :after (lambda () (just-one-space)))
  (advice-add 'sp-add-to-previous-sexp :after (lambda () (sp-forward-sexp)))
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
  (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)
  :bind
  (:map
   smartparens-mode-map
   ("<C-backspace>" . sp-backward-kill-sexp)
   ("M-b" . sp-backward-sexp)
   ("M-d" . sp-kill-sexp)
   ("M-f" . sp-forward-sexp)
   ("M-h" . sp-select-next-thing)
   ("M-k" . sp-kill-hybrid-sexp)
   ("M-t" . sp-add-to-previous-sexp)))

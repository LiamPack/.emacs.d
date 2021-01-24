(require 'use-package)

(use-package isearch
  :diminish
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  :bind (:map minibuffer-local-isearch-map
              ("M-/" . isearch-complete-edit)
              :map isearch-mode-map
              ("C-g" . isearch-cancel)       ; instead of `isearch-abort'
              ("M-/" . isearch-complete)))

(use-package replace
  :config
  (setq list-matching-lines-jump-to-current-line t)
  :hook ((occur-mode-hook . hl-line-mode)
         (occur-mode-hook . (lambda ()
                              (toggle-truncate-lines t))))
  :bind (("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

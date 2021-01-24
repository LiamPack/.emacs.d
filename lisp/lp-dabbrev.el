(require 'use-package)
(use-package dabbrev
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  :bind (("M-/" . dabbrev-expand)
         ("M-?" . dabbrev-completion)))

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name))
  (setq hippie-expand-verbose t)
  (setq hippie-expand-dabbrev-skip-space nil)
  (setq hippie-expand-dabbrev-as-symbol t)
  (setq hippie-expand-no-restriction t)
  :bind ("C-M-_" . hippie-expand))

(provide 'lp-dabbrev)

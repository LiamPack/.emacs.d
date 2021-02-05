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
  ;; :bind (("M-/" . dabbrev-expand)
  ;;        ("M-?" . dabbrev-completion))

  )

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
  ;; :bind ("C-M-_" . hippie-expand)
  )


(use-package company
  :straight t
  :config
  (setq company-idle-delay 20)
  (setq company-dabbrev-downcase 0)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-other-buffers t)
  (setq company-auto-complete nil)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  (global-set-key (kbd "M-/") 'company-complete)
  ;; (global-set-key (kbd "C-M-_") 'company-complete)
  ;; (global-set-key (kbd "C-c C-y") 'company-yasnippet)
  (setq company-backends '(company-files company-keywords
                                         company-capf company-dabbrev-code company-etags
                                         company-dabbrev))
  (global-company-mode 1)

  ;; (add-to-list 'company-backends 'company-capf t)
  )


(provide 'lp-dabbrev)

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
  :bind (("TAB" . dabbrev-expand)
         ("C-M-_" . dabbrev-completion))
  )
(use-package company
  :straight t
  :diminish
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

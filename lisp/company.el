(require 'use-package)

(use-package company
  :straight t
  :diminish company-mode
  :config
  (setq auto-revert-check-vc-info 'nil)
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  (defun mars/company-backend-with-yas (backends) "Add :with company-yasnippet to company BACKENDS. Taken from https://github.com/syl20bnr/spacemacs/pull/179." (if (and (listp backends) (memq 'company-yasnippet backends)) backends (append (if (consp backends) backends (list backends)) '(:with company-yasnippet))))

  (defvar my-company-backends nil
    "A list of my company backends")
  (setq my-company-backends
	'(company-auctex-labels
          company-auctex-bibs
          (company-auctex-macros company-auctex-symbols company-auctex-environments)
          (company-math-symbols-latex company-math-symbols-unicode)
          company-ispell
          (company-semantic
           company-clang company-c-headers)
          ;;company-matlab-shell
          company-bbdb
          company-elisp
          ac-js2-company
          company-nxml
          company-css
          company-cmake
          company-capf
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse
          company-files
          company-dabbrev
          company-yasnippet))
  (setq company-backends my-company-backends)

  ;; add yasnippet to all backends
  (setq company-backends
	(mapcar #'mars/company-backend-with-yas company-backends))

  )

(use-package company-statistics         ; Sort company candidates by statistics
  :straight t
  :after company
  :config (company-statistics-mode))

(provide 'use-company)

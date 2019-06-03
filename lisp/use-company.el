(require 'use-package)

(use-package company
  :ensure t
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
  )

(use-package company-quickhelp          ; Show help in tooltip
  ;;:disabled t                           ; M-h clashes with mark-paragraph
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :bind (:map company-mode-map
              ([remap complete-symbol] . helm-company)
              ([remap completion-at-point] . helm-company)
              :map company-active-map
              ("C-:" . helm-company)))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))


;; (defvar my-company-backends nil
;;   "A list of my company backends")
;; (setq my-company-backends
;;       '(company-auctex-labels
;;         company-auctex-bibs
;;         (company-auctex-macros company-auctex-symbols company-auctex-environments)
;;         (company-math-symbols-latex company-math-symbols-unicode)
;;         company-ispell
;;         (company-semantic
;;          company-clang company-c-headers)
;;         ;;company-matlab-shell
;;         company-bbdb
;;         company-elisp
;;         ac-js2-company
;;         company-nxml
;;         company-css
;;         company-eclim
;;         company-xcode
;;         company-cmake
;;         company-capf
;;         (company-dabbrev-code company-gtags company-etags company-keywords)
;;         company-oddmuse
;;         company-files
;;         company-dabbrev
;;         company-yasnippet))
;; (setq company-backends my-company-backends)

(provide 'use-company)

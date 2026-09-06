;;; isearch, of course
(lp-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-lazy-highlight t)

  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-suffix-format " {%s/%s}")
  (setq lazy-count-prefix-format nil)
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(lp-emacs-builtin-package 'electric
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((?\[ . ?\])
          (?{ . ?})
	  (?\( . ?\))
	  (?$ . ?$)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace t)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  
  (electric-pair-mode t)
  (electric-quote-mode t)
  (electric-indent-mode t)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode)
  ;; (dolist (hook (list #'electric-indent-mode
  ;;                     ;; #'electric-quote-mode
  ;;                     #'electric-pair-mode
  ;;                     ))
  ;;   (add-hook 'text-mode-hook hook))
  )

(lp-emacs-builtin-package 'replace
  (define-key global-map (kbd "M-s M-o") 'multi-occur)
  (define-key occur-mode-map (kbd "t") 'toggle-truncate-lines)
  (add-hook 'occur-mode-hook #'(lambda () (interactive) (toggle-truncate-lines t)))
  (add-hook 'occur-mode-hook #'hl-line-mode)

  (setq list-matching-lines-jump-to-current-line t))

;;; Right-click menus for emacs
(lp-emacs-elpa-package 'embark
  (define-key global-map (kbd "C->") 'embark-become)
  (define-key global-map (kbd "C-.") 'embark-act)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-confirm-act-all t)
  (setq embark-quit-after-action nil)

  ;; I prefer the non-verbose indicators; that buffer gets fucking
  ;; huge
  (setq embark-indicators
	'(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

  ;; but if i ever reactivate verbose indicators:
  (setq embark-mixed-indicator-both t)
  (setq embark-mixed-indicator-delay 1))

(lp-emacs-elpa-package 'embark-consult
  (define-key embark-collect-mode-map (kbd "o") 'consult-preview-at-point)
  (define-key embark-collect-mode-map (kbd "C-o") 'consult-preview-at-point))

;;; Rectangle editing
(lp-emacs-builtin-package 'rect
  (let ((map rectangle-mark-mode-map))
    (define-key map (kbd "t") #'string-rectangle)
    (define-key map (kbd "o") #'open-rectangle)
    (define-key map (kbd "c") #'clear-rectangle)
    (define-key map (kbd "n") #'rectangle-number-lines)
    (define-key map (kbd "x") #'rectangle-exchange-point-and-mark)
    (define-key map (kbd "k") #'kill-rectangle)
    (define-key map (kbd "y") #'copy-rectangle-as-kill)
    (define-key map (kbd "*") #'calc-grab-rectangle)
    (define-key map (kbd ":") #'calc-grab-sum-down)
    (define-key map (kbd "_") #'calc-grab-sum-across)))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key global-map (kbd "M-Q") #'unfill-paragraph)

(provide 'lp-editing)

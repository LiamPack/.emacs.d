;;; isearch, of course
(lp-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
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
  
  ;; (electric-pair-mode t)
  ;; (electric-quote-mode t)
  ;; (electric-indent-mode t)
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

;;; Simple QoL substitutions
(lp-emacs-elpa-package 'substitute
  (setopt substitute-highlight t)
  (setq substitute-fixed-letter-case nil)
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)
  (define-key global-map (kbd "C-c r") #'substitute-prefix-map))

;;; Avy jumping
(lp-emacs-elpa-package 'avy
  (avy-setup-default) ; binds C-' in the isearch map
  (let ((map global-map))
    (define-key global-map (kbd "C-' C-l") 'avy-goto-line)
    ;; (define-key global-map (kbd "C-' C-'" 'avy-timer)) Just use avy in an isearch
    (define-key global-map (kbd "C-' C-a") 'avy-goto-word-0-above)
    (define-key global-map (kbd "C-' C-n") 'avy-goto-word-0-below)
    
    (define-key global-map (kbd "M-'") 'avy-resume))


  (setq avy-timeout-seconds 0.4)
  (setq avy-background nil))

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
(define-key global-map (kbd "C-M-l") #'mark-line)

;;; TODO useful editing functions to implement:
;; - [X] unfill paragraph
;; - [x] mark-line
;; - [x] mark-word C-M-SPC should be fine, or embark
;; - goto-random-line
;; - [x] kill-inside-sexp  embark w
;; - [x] mark-inside-sexp  embark SPC
;; - unwrap-sexp 
;; - unwrap-mark-sexp 
;; - [x] upcase-dwim (exists)
;; - [x] downcase-dwim (exists)

;; TODO: someday, `meow' https://github.com/meow-edit/meow
;; (lp-emacs-elpa-package 'meow
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; SPC j/k will run the original command in MOTION state.
;;      '("j" . "H-j")
;;      '("k" . "H-k")
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;;   (meow-setup)
;;   (meow-global-mode 1)
  
;;   )


(provide 'lp-editing)

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

(lp-emacs-builtin-package 'replace
  (define-key global-map (kbd "M-s M-o") 'multi-occur)
  (define-key occur-mode-map (kbd "t") 'toggle-truncate-lines)
  (add-hook 'occur-mode-hook #'(lambda () (interactive) (toggle-truncate-lines t)))
  (add-hook 'occur-mode-hook #'hl-line-mode)

  (setq list-matching-lines-jump-to-current-line t))

;;; Still trying to get used to jumping around
(lp-emacs-elpa-package 'avy
  (avy-setup-default) ; binds C-' in the isearch map
  (define-key global-map (kbd "C-'") 'avy-goto-char-timer)
  (define-key global-map (kbd "M-'") 'avy-resume))

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
  (replace-string-in-region
   "\n" " "
   (progn (backward-paragraph) (forward-line) (point)) (progn (forward-paragraph) (previous-line) (point))))

(define-key global-map (kbd "M-Q") #'unfill-paragraph)

;;; TODO useful editing functions to implement:
;; - [X] unfill paragraph
;; - mark-line
;; - mark-word
;; - goto-random-line
;; - kill-inside-sexp 
;; - mark-inside-sexp 
;; - unwrap-sexp 
;; - unwrap-mark-sexp 
;; - upcase-dwim
;; - downcase-dwim

(provide 'lp-editing)

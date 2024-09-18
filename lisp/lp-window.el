(define-key global-map (kbd "M-o") 'other-window)
(define-key global-map (kbd "M-O") (lambda () (interactive) (other-window -1)))

;; (lp-emacs-builtin-package 'ibuffer                    ; Better buffer list
;;   (define-key global-map [remap list-buffers] #'ibuffer)
;;   ;; as always, from prot:
;;   (setq ibuffer-expert t)
;;   (setq ibuffer-display-summary nil)
;;   (setq ibuffer-show-empty-filter-groups t)
;;   (setq ibuffer-movement-cycle nil)
;;   (setq ibuffer-default-sorting-mode 'filename/process)
;;   (setq ibuffer-use-header-line t)
;;   (setq ibuffer-default-shrink-to-minimum-size nil)
  
;;   (setq ibuffer-saved-filter-groups nil)
;;   (setq ibuffer-old-time 48)
;;   (add-hook 'ibuffer-mode-hook #'hl-line-mode)
;;   (define-key global-map (kbd "C-x C-b") #'ibuffer)
;;   (let ((map ibuffer-mode-map))
;;     (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
;;     (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
;;     (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
;;     (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
;;     (define-key map (kbd "/ g") #'ibuffer-filter-by-content))
;;   )

(lp-emacs-elpa-package 'beframe
  (setq beframe-functions-in-frames '(project-prompt-project-dir))
  (setq beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))

  (beframe-mode 1)

  ;; This is just an example.  We do not define any key bindings.  You
  ;; do not need this command if you enable `beframe-mode', as
  ;; `switch-to-buffer' only shows a list of beframed buffers.
  (define-key global-map (kbd "C-x B") #'beframe-switch-buffer)

  (define-key global-map (kbd "C-x f") #'other-frame-prefix) ; override `set-fill-column'
  (define-key global-map (kbd "C-x C-b") #'beframe-buffer-menu)

  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe--consult-source
      `( :name     "Frame-specific buffers (current frame)"
	 :narrow   ?F
	 :category buffer
	 :face     beframe-buffer
	 :history  beframe-history
	 :items    ,#'beframe--buffer-names
	 :action   ,#'switch-to-buffer
	 :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe--consult-source)))

;; Thank you prot (see
;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779)
(lp-emacs-builtin-package 'window
  (let ((map global-map))
    (define-key map (kbd "C-x C-o") #'display-buffer)
    (define-key map (kbd "C-1") #'delete-other-windows)
    (define-key map (kbd "C-2") #'split-window-below)
    (define-key map (kbd "C-3") #'split-window-right)
    (define-key map (kbd "C-0") #'delete-window))
  (setq switch-to-prev-buffer-skip 'this)
  (customize-set-variable 'even-window-sizes nil)

  (customize-set-variable
   'display-buffer-base-action
   '(
     ;; (display-buffer-reuse-window display-buffer-same-window)
     ;; i'd really like this to work, but so many things fuck with this
     ;; that its annoying. e.g. magit-commit
     ;; (reusable-frames . t)
     ))

  (setq display-buffer-alist
	`(("\\*\\(e?shell\\|v?term\\|.*geiser.*\\|\\*julia\\)\\*"
	   (display-buffer-below-selected)
	   (window-height . 0.3))
	  ("\\*\\(Flymake diagnostics\\|Package-Lint\\|flycheck\\).*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 0))
	  ("\\*Messages.*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 1))     
	  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\|compilation\\|\\)\\*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 2))
	  ("\\*\\(.* # Help.*\\|Help\\)\\*" ; See the hooks for `visual-line-mode'
	   (display-buffer-reuse-mode-window display-buffer-in-side-window)
	   (window-width . 0.25)
	   (side . top)
	   (slot . 0))
	  ("\\*\\(Output\\|Register Preview\\).*"
	   (display-buffer-reuse-mode-window display-buffer-at-bottom))     
	  ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
	   (display-buffer-reuse-mode-window display-buffer-below-selected)
	   ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
	   ;; the height is not known in advance.
	   (window-height . 0.4))
	  ("magit: .*"
	   (display-buffer-reuse-mode-window display-buffer-below-selected)
	   (window-height . 0.4))
	  ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
	   (display-buffer-reuse-mode-window display-buffer-below-selected)
	   (window-height . fit-window-to-buffer))))

  (defvar resize-window-repeat-map
    (let ((map (make-sparse-keymap)))
      ;; Standard keys:
      (define-key map "}" 'enlarge-window)
      (define-key map ">" 'enlarge-window-horizontally)
      (define-key map "<" 'shrink-window-horizontally) ; prot note: those three are C-x KEY
      ;; Additional keys:
      (define-key map "{" 'shrink-window) ; prot note: this is not bound by default
      map)
    "Keymap to repeat window resizing commands.  Used in `repeat-mode'.")

  (put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
  (put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
  (put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
  (put 'shrink-window 'repeat-map 'resize-window-repeat-map)

  (setq fit-window-to-buffer-horizontally t)

  (let ((map global-map))
    (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
    (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
    (define-key map (kbd "C-`") #'next-buffer)
    (define-key map (kbd "C-~") #'previous-buffer)
    (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
    (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
    (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "C-x }") #'enlarge-window)
    (define-key map (kbd "C-x {") #'shrink-window)
    (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
    (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)

  (define-key global-map (kbd "C-x u") #'undelete-frame) ; I use only C-/ for `undo'
  (undelete-frame-mode 1))

(lp-emacs-builtin-package 'winner
  (winner-mode 1)
  (let ((map global-map)) 
    (define-key map (kbd "C-x <right>") #'winner-redo)
    (define-key map (kbd "C-x <left>") #'winner-undo)))

(provide 'lp-window)

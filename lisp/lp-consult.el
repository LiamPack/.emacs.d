(require 'use-package)

(use-package consult
  :straight t
  :bind
  ;; ("M-y" . consult-yank-pop)
  ;; ("M-g l" . consult-line)    ("M-g M-l" . consult-line)
  ;; ("M-g i" . consult-imenu)   ("M-g M-i" . consult-imenu)
  ;; ("M-g o" . consult-outline) ("M-g M-o" . consult-outline)
  ;; ("M-g m" . consult-mark)
  ;; ("M-g k" . consult-global-mark)
  ;; ("M-g e" . consult-error)
  ("C-x b" . consult-buffer)
  ;; ("C-c k" . consult-ripgrep)
  ;; ("M-K" . consult-keep-lines)
  ;; ("M-X" . consult-mode-command)
  ;; ("C-c f" . consult-focus-lines)
  ;; ("M-#" . consult-register-load)
  ;; ("M-'" . consult-register-store)

  ;; (:map consult-narrow-map
  ;;       ("?" . consult-narrow-help))
  ;; (:map minibuffer-local-map
  ;;       ("M-r" . consult-history)
  ;;       ("M-s"))
  ;; NOTE: check `embark-consult' for previews that can be used with the
  ;; default minibuffer and Embark collections.
  ;; :bind (("C-x M-:" . consult-complex-command)
  ;;        ("C-x M-m" . consult-minor-mode-menu)
  ;;        ("C-x M-k" . consult-kmacro)
  ;;        ("M-g g" . consult-goto-line)
  ;;        ("M-g M-g" . consult-goto-line)

  ;;        ("M-K" . consult-keep-lines)  ; M-S-k is similar to M-S-5 (M-%)

  ;;        ("M-s m" . consult-mark)
  ;;        )
  :init
  (setq consult-goto-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "e") 'consult-compile-error)
          (define-key map (kbd "f") 'consult-flymake)               ;; Alternative: consult-flycheck
          (define-key map (kbd "g") 'consult-goto-line)             ;; orig. goto-line
          (define-key map (kbd "M-g") 'consult-goto-line)           ;; orig. goto-line
          (define-key map (kbd "o") 'consult-outline)               ;; Alternative: consult-org-heading
          (define-key map (kbd "m") 'consult-mark)
          (define-key map (kbd "k") 'consult-global-mark)
          (define-key map (kbd "i") 'consult-imenu)
          map))

  (setq consult-register-map
        (let ((map (make-sparse-keymap)))
          ;; Custom M-# bindings for fast register access
          (define-key map (kbd "l") 'consult-register-load)
          (define-key map (kbd "s") 'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
          (define-key map (kbd "r") 'consult-register)
          map))
  (setq consult-mode-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "h") 'consult-history)
          (define-key map (kbd "m") 'consult-mode-command)
          (define-key map (kbd "k") 'consult-kmacro)
          map))

  (setq consult-search-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "f") 'consult-find)
          (define-key map (kbd "F") 'consult-locate)
          (define-key map (kbd "g") 'consult-grep)
          (define-key map (kbd "G") 'consult-git-grep)
          (define-key map (kbd "r") 'consult-ripgrep)
          (define-key map (kbd "l") 'consult-line)
          (define-key map (kbd "L") 'consult-line-multi)
          (define-key map (kbd "m") 'consult-multi-occur)
          (define-key map (kbd "k") 'consult-keep-lines)
          (define-key map (kbd "u") 'consult-focus-lines)
          map))
  (global-set-key (kbd "M-s") consult-search-map)
  (global-set-key (kbd "M-j") consult-goto-map)

  :config
  (setq consult-line-numbers-widen t)
  (setq consult-project-root-function #'projectile-project-root)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key "<")

  ;; Registers' setup -- From Consult's README
  ;;
  ;; This gives a consistent display for `consult-register',
  ;; `consult-register-load', `consult-register-store', and the Emacs
  ;; built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Tweak the register preview window.
  ;; * Sort the registers
  ;; * Hide the mode line
  ;; * Resize the window, such that the contents fit exactly
  ;; (advice-add #'register-preview #'consult-register-window)
  (advice-add #'register-preview :around
              (lambda (fun buffer &optional show-empty)
                (let ((register-alist (seq-sort #'car-less-than-car register-alist)))
                  (funcall fun buffer show-empty))
                (when-let (win (get-buffer-window buffer))
                  (with-selected-window win
                    (setq-local mode-line-format nil)
                    (setq-local window-min-height 1)
                    (fit-window-to-buffer)))))

  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history)
  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref))



;; (define-key map (kbd "C-v k") 'consult-ag)
;; (define-key map (kbd "C-v l") 'consult-line)
;; (define-key map (kbd "C-v k") 'consult-imenu)
;; (define-key map (kbd "C-v k") 'consult-imenu-multi)
;; (define-key map (kbd "C-v k") 'consult-outline)
;; (define-key map (kbd "C-v k") 'consult-ripgrep)
;; (define-key map (kbd "C-v k") 'consult-git-grep)
;; (define-key map (kbd "C-v k") 'consult-yank-pop)
;; (define-key map (kbd "C-v k") 'consult-find)
;; (define-key map (kbd "C-v k") 'consult-locate)
;; (define-key map (kbd "C-v k") 'consult-xref)
;; (define-key map (kbd "C-v k") 'consult-focus-lines)
;; (define-key map (kbd "C-v k") 'consult-global-mark)
;; (define-key map (kbd "C-v k") 'consult-register-store)
;; (define-key map (kbd "C-v k") 'consult-register-load)
;; (define-key map (kbd "C-v k") 'consult-register)
;; (define-key map (kbd "C-v k") 'consult-buffer)
;; (define-key map (kbd "C-v k") 'consult-preview-at-point)
;; (define-key map (kbd "C-v k") 'consult-completion-in-region)
;; (define-key map (kbd "C-v k") 'consult-ag)

(provide 'lp-consult)

(require 'use-package)

(use-package consult
  :straight t
  :bind
  ("M-y" . consult-yank-pop)
  ("M-g l" . consult-line)    ("M-g M-l" . consult-line)
  ("M-g i" . consult-imenu)   ("M-g M-i" . consult-imenu)
  ("M-g o" . consult-outline) ("M-g M-o" . consult-outline)
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g e" . consult-error)
  ("C-x b" . consult-buffer)
  ("C-c k" . consult-ripgrep)
  ("M-K" . consult-keep-lines)
  ("M-X" . consult-mode-command)
  ("C-c f" . consult-focus-lines)
  (:map consult-narrow-map
        ("?" . consult-narrow-help))
  (:map minibuffer-local-map
        ("M-r" . consult-history)
        ("M-s"))
  ;; NOTE: check `embark-consult' for previews that can be used with the
  ;; default minibuffer and Embark collections.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)

         ("M-K" . consult-keep-lines)  ; M-S-k is similar to M-S-5 (M-%)

         ("M-s m" . consult-mark)
         )
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-preview)
  :config
  (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")

  ;; Registers' setup -- From Consult's README
  ;;
  ;; This gives a consistent display for `consult-register',
  ;; `consult-register-load', `consult-register-store', and the Emacs
  ;; built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  ;; Tweak the register preview window.
  ;; * Sort the registers
  ;; * Hide the mode line
  ;; * Resize the window, such that the contents fit exactly
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
        'slime-repl-input-history))

(use-package prot-consult
  :straight (:type built-in)
  :after (consult)
  :config
  (setq consult-project-root-function #'prot-consult-project-root)
  (setq prot-consult-command-centre-list
        '(consult-line
          prot-consult-line
          consult-mark))
  (setq prot-consult-command-top-list
        '(consult-outline
          consult-imenu
          prot-consult-outline
          prot-consult-imenu))
  (prot-consult-set-up-hooks-mode 1)
  :bind (("M-s i" . prot-consult-imenu)
         ("M-s f" . prot-consult-fd)
         ("M-s s" . prot-consult-outline)    ; M-s o is `occur'
         ("M-s y" . prot-consult-yank)
         ("M-s l" . prot-consult-line)))

(provide 'lp-consult)

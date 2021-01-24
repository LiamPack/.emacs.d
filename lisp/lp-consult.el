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
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s m" . consult-multi-occur)
  ("M-X" . consult-mode-command)
  ("C-c b" . consult-buffer)
  ("C-c k" . consult-keep-lines)
  ("C-c f" . consult-focus-lines)
  (:map minibuffer-local-map
        ("M-r" . consult-history)
        ("M-s"))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview-function #'consult-register-preview)
  :config
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history))

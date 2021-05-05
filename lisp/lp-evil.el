(require 'use-package)

(use-package general
  :straight t)

(use-package evil
  :straight t
  :after (undo-tree general key-chord)
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-mode-line-format nil
        evil-insert-state-cursor '(bar "White")
        evil-visual-state-cursor '(box "#F86155"))
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-undo-system 'undo-tree)
  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  ;;  (define-key my-leader-map "b" 'list-buffers)

  ;; change the "leader" key to space
  (define-key evil-normal-state-map "," 'evil-repeat-find-char-reverse)
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)

  ;; general.el can automate the process of prefix map/command creation
  (general-evil-setup)
  (general-nmap
    :prefix "SPC"
    :prefix-map 'my-leader-map
    
    "f f" 'find-file
    "f o" 'find-file-other-window

    "p f" 'projectile-find-file
    "p p" 'projectile-switch-project
    "p c" 'projectile-compile-project
    "p j" 'projectile-find-tag
    "p a" 'projectile-find-other-file
    "p A" 'projectile-find-other-file-other-window
    "p x g" 'projectile-run-gdb
    "p x v" 'projectile-run-vterm

    "w =" 'balance-windows
    "w k" 'lp/kill-current-buffer
    "w m" 'delete-other-windows
    
    "1" 'delete-other-windows
    "2" 'lp/split-window-below-and-switch
    "3" 'lp/split-window-right-and-switch
    "`" '(lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1)))
    "o" 'ace-window

    "a" 'embark-act
    "b" 'consult-buffer
    "B" 'ibuffer
    "s" 'save-buffer
    "F" 'lsp-format-buffer

    "l" 'consult-line
    "i" 'consult-imenu
    "O" 'consult-outline
    "m" 'consult-global-mark
    "k" 'consult-ripgrep
    "Y" 'consult-yank-pop

    "r w" 'window-configuration-to-register
    "r p" 'point-to-register
    "r f" 'framset-to-register
    "r s" 'consult-register-store
    "r l" 'consult-register-load
    "r j" 'jump-to-register

    "e n" 'next-error
    "e p" 'previous-error
    "e d" 'flycheck-display-error-at-point
    "e l" 'consult-flycheck
    "e L" 'flycheck-list-errors
    "e c" 'flycheck-compile
    "e w" 'flycheck-copy-errors-as-kill

    "g b" 'gud-break
    "g <" 'gud-up
    "g >" 'gud-down
    "g n" 'gud-next
    "g s" 'gud-step
    "g c" 'gud-cont
    "g p" 'gud-print
    "g d" 'gud-remove
    "g l" 'gud-refresh
    "g e" 'gud-statement
    ;;   "M-K" 'consult-keep-lines
    ;;   "M-X" 'consult-mode-command
    ;;"C-c f" 'consult-focus-lines
    ))

(use-package evil-collection
  :straight t
  :diminish
  :after evil
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :diminish
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)

  (evil-escape-mode +1)
  )
(use-package evil-snipe
  :straight t
  :diminish
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  ;;(append evil-snipe-disabled-modes 'Info-mode 'calc-mode 'treemacs-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package evil-surround
  :straight t
  :config (global-evil-surround-mode 1))

(provide 'lp-evil)

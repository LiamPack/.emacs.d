(require 'use-package)

(use-package general
  :straight t)

(use-package evil
  :straight t
  :init
  (setq evil-search-module 'isearch)

  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-mode-line-format nil)
  ;; (setq evil-mode-line-format nil
  ;;       evil-insert-state-cursor '(bar "White")
  ;;       evil-visual-state-cursor '(box "#F86155"))
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; general.el can automate the process of prefix map/command creation
  (general-evil-setup)
  (general-nmap
    :prefix "SPC"
    :prefix-map 'my-leader-map

    ;; "f f" 'find-file
    ;; "f o" 'find-file-other-window
    ;; "f r" 'prot-recentf-recent-files
    ;; "f d" 'prot-recentf-recent-dirs
    "j" 'org-roam-dailies-find-today
    "s" 'isearch-forward

    "a" 'embark-act
    "b" 'consult-buffer

    "_" 'balance-windows
    "-" 'fit-window-to-buffer
    "+" 'balance-windows-area
    "q" 'window-toggle-side-windows
    "w k" 'lp/kill-current-buffer
    "w m" 'delete-other-windows

    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'lp/split-window-below-and-switch
    "3" 'lp/split-window-right-and-switch
    "`" '(lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1)))
    "o" 'ace-window

    "B" 'ibuffer
    "F" 'lsp-format-buffer

    "]" 'isearch-forward
    "[" 'isearch-backward
    ;; "s ." 'isearch-forward-symbol-at-point
    ;; "s h r" 'highlight-regexp
    ;;
    "5" 'query-replace
    "%" 'query-replace-regexp

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

    "d l" 'dap-debug-last
    "d d" 'dap-debug
    "d b a" 'dap-breakpoint-add
    "d b c" 'dap-breakpoint-condition
    "d b d" 'dap-breakpoint-condition
    "d c" 'dap-continue
    "d n" 'dap-next
    "d s" 'dap-step-in
    "d r" 'dap-ui-repl

    "n p" 'org-gcal-post-at-point
    "n i" '(lambda () (interactive) (org-time-stamp-inactive '(16)))

    ;; "t b" 'switch-to-buffer-other-tab
    ;; "t d" 'dired-other-tab
    ;; "t f" 'find-file-other-tab
    ;; "t n" 'tab-next
    ;; "t p" 'tab-previous
    ;; "t 0" 'tab-close
    ;; "t 1" 'tab-close-other
    ;; "t 2" 'tab-bar-new-tab
    ;; "t l" 'tab-list

    "u f" 'org-roam-find-file
    "u c" 'org-roam-capture
    "u i" 'org-roam-insert
    "u r" 'org-roam
    "u I" 'org-roam-insert-immediate
    "u g" 'org-roam-graph
    "u o" 'org-roam-jump-to-index
    "u d" 'deft
    "u t" 'org-roam-tag-add
    "p" projectile-command-map
    "f" consult-search-map
    "r" consult-register-map
    "t" consult-mode-mode-map)
  :config
  (evil-mode 1)
  (setq evil-undo-system 'undo-tree)
  (evil-set-initial-state 'deft-mode 'emacs)
  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'delve-mode 'emacs)

  ;;  (define-key my-leader-map "b" 'list-buffers)

  ;; change the "leader" key to space
  (define-key evil-normal-state-map "," 'evil-repeat-find-char-reverse)
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-,") 'xref-pop-marker-stack)
  (define-key evil-normal-state-map (kbd "C-M-.") 'xref-find-apropos)

  )

(use-package evil-collection
  :straight t
  :diminish (evil-collection-unimpaired-mode  global-evil-collection-unimpaired-mode)
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :diminish
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode tab-switcher-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)

  (evil-escape-mode +1)
  )
(use-package evil-snipe
  :straight t
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  ;;(append evil-snipe-disabled-modes 'Info-mode 'calc-mode 'treemacs-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package evil-surround
  :straight t
  :diminish
  :config (global-evil-surround-mode 1))

(provide 'lp-evil)

(require 'use-package)
(use-package ivy
  :straight t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d "))

(use-package ivy-hydra                  ; Hydra bindings for ivy buffer
  :straight t
  :after ivy
  :bind (:map ivy-minibuffer-map
              ("C-o" . hydra-ivy/body)))

;; (use-package counsel
;;   :straight t
;;   :bind (("C-x f" . counsel-find-file)
;;          ("C-? f" . counsel-describe-function)
;;          ("C-? v" . counsel-describe-variable)
;;          ("M-x" . counsel-M-x)
;;          ("C-c o" . counsel-recentf)
;;          ("C-x l" . counsel-locate))
;;   :config
;;   )

(use-package counsel                    ; Ivy-powered commands
  :straight t
  :after ivy
  :diminish counsel-mode
  :init (counsel-mode)
  :bind (([remap execute-extended-command]  . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ([remap switch-to-buffer] . counsel-switch-buffer)
	 ("C-c k"                          . counsel-rg)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ;;("C-c i 8"                        . counsel-unicode-char)
         ("C-c f m"                        . counsel-imenu))
  :config
  (setf ivy-wrap t
	ivy-re-builders-alist '((t . ivy--regex-plus)))
  (progn
    (unbind-key "C-x p" counsel-mode-map)
    (global-set-key (kbd "C-x p") 'pop-to-mark-command)))

(use-package flx                        ; flex searching (fuzzy)
  :straight t)

(use-package avy ; Maybe also look into iy- whatever for this. interesting extension/alternative though
  :straight t
  :bind (("C-\'" . avy-goto-char)
         ("C-\"" . avy-goto-char-timer)
         ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

(provide 'use-ivys)

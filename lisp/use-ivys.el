(require 'use-package)

;; ivy stuff
(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init  (with-eval-after-load 'ido
           (ido-mode -1)
           ;; Enable ivy
           (ivy-mode 1))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d "))

(use-package ivy-hydra                  ; Hydra bindings for ivy buffer
  :ensure t
  :after ivy
  :bind (:map ivy-minibuffer-map
              ("C-o" . hydra-ivy/body)))

;; (use-package counsel
;;   :ensure t
;;   :bind (("C-x f" . counsel-find-file)
;;          ("C-? f" . counsel-describe-function)
;;          ("C-? v" . counsel-describe-variable)
;;          ("M-x" . counsel-M-x)
;;          ("C-c o" . counsel-recentf)
;;          ("C-x l" . counsel-locate))
;;   :config
;;   )

(use-package counsel                    ; Ivy-powered commands
  :ensure t
  :diminish counsel-mode
  :init (counsel-mode)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ;;("C-c i 8"                        . counsel-unicode-char)
         ("C-c f a"                        . counsel-ag)
         ("C-c f m"                        . counsel-imenu))
  :config
  (setq counsel-find-file-at-point t)  )

(use-package swiper
  :ensure t
  :defer t
  ;; :bind (:map isearch-mode-map
  ;;             ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind (("C-c C-r" . swiper)
         ("C-c C-s" . counsel-grep-or-swiper)))

(use-package flx
  :ensure t)

(use-package swiper
  :ensure t
  :init (ivy-mode 1)
  :config
  (setf ivy-wrap t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (define-key ivy-minibuffer-map (kbd "C-s") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done))


(provide 'use-ivys)

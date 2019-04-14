(require 'use-package)

;; ivy stuff
(use-package smex
  :ensure t
  :disabled t
  :config
  (smex-initialize))

(use-package ivy
  :ensure t
  :disabled t
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
  :disabled t
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
  :disabled t
  :diminish counsel-mode
  :init (counsel-mode)
  :bind (([remap execute-extended-command]  . counsel-M-x)
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

  (progn
    (unbind-key "C-x p" counsel-mode-map)
    (global-set-key (kbd "C-x p") 'pop-to-mark-command)))

(use-package swiper ; don't really use this one much. maybe better than isearch?
  :ensure t
  :disabled t
  :defer t
  ;; :bind (:map isearch-mode-map
  ;;             ("M-i" . swiper-from-isearch)) ; isearch > swiper
  :bind (("C-c C-r" . swiper)
         ("C-c C-s" . counsel-grep-or-swiper)))


(use-package flx                        ; flex searching (fuzzy)
  :ensure t)

(use-package swiper
  :ensure t
  :disabled t
  :init (ivy-mode 1)
  :config
  (setf ivy-wrap t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (define-key ivy-minibuffer-map (kbd "C-s") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-alt-done))

(use-package avy ; Maybe also look into iy- whatever for this. interesting extension/alternative though
  :ensure t
  :bind (("C-\'" . avy-goto-char)
         ("C-\"" . avy-goto-char-timer)
         ("C-c C-j" . avy-resume))
  :config
  (avy-setup-default))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)

  ;; helm is a little much for me
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)

  (helm-mode 1)

  ;; going to want to use M-x with helm's powerful interface
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; We also are going to want to use the kill-ring feature
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; helm also has a great interface to a number of
  ;; different buffers and stuff like that
  (global-set-key (kbd "C-x b") 'helm-mini)

  ;; Another place to stick helm in. fuzzy matching,
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  ;; We also want helm-occur to not be on a horrible keybind
  (global-set-key (kbd "C-c h o") 'helm-occur)

  ;; REGISTERS!
  (global-set-key (kbd "C-c h x") 'helm-register)
  (with-eval-after-load 'helm
    (setq helm-always-two-windows nil)
    (setq helm-display-buffer-default-height 15)
    (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))
  )


(provide 'use-ivys)

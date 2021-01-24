                                        ; image manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;; C-c + / C-c -: Zoom in/out image.

;; C-c M-m: Adjust image to current frame size.

;; C-c C-x C-s: Save current image.

;; C-c M-r / C-c M-l: Rotate image.

;; C-c M-o: Show image image+ have not modified.

(use-package image+
  :disabled t
  :straight t
  :defer t
                                        ;    :load-path "~/elisp/Emacs-imagex"
  :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
  :init (progn (imagex-global-sticky-mode)
               (imagex-auto-adjust-mode 1)
               (defhydra imagex-sticky-binding (global-map "C-x C-l")
                 "Manipulating Image"
                 ("+" imagex-sticky-zoom-in "zoom in")
                 ("-" imagex-sticky-zoom-out "zoom out")
                 ("M" imagex-sticky-maximize "maximize")
                 ("O" imagex-sticky-restore-original "restore original")
                 ("S" imagex-sticky-save-image "save file")
                 ("r" imagex-sticky-rotate-right "rotate right")
                 ("l" imagex-sticky-rotate-left "rotate left"))))
(use-package pdf-tools
  :disabled t
  :straight t
  :config
  (pdf-tools-install)
  (setq pdf-view-resize-factor 1.05)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)
  (setq-default pdf-view-display-size 'fit-page)

  ;; (global-unset-key (kbd "<double-down-mouse-4>"))
  ;; (global-unset-key (kbd "<double-down-mouse-5>"))
  ;; (global-unset-key (kbd "<double-down-mouse-6>"))
  ;; (global-unset-key (kbd "<double-down-mouse-7>"))
  ;; (global-unset-key (kbd "<down-mouse-4>"))
  ;; (global-unset-key (kbd "<down-mouse-5>"))
  ;; (global-unset-key (kbd "<down-mouse-6>"))
  ;; (global-unset-key (kbd "<down-mouse-7>"))
  ;; (global-unset-key (kbd "<mouse-4>"))
  ;; (global-unset-key (kbd "<mouse-5>"))
  ;; (global-unset-key (kbd "<mouse-6>"))
  ;; (global-unset-key (kbd "<mouse-7>"))

  (define-key pdf-view-mode-map (kbd "<double-mouse-7>") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "<double-mouse-6>") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "C-S-n") (lambda ()  (interactive) (pdf-view-next-line-or-next-page 3)))
  (define-key pdf-view-mode-map (kbd "C-S-p") (lambda ()  (interactive) (pdf-view-previous-line-or-previous-page 3)))
  ;; (defun up-one () (interactive) (scroll-up 1))
  ;; (defun down-one () (interactive) (scroll-down 1))
  ;; (defun left-one () (interactive) (scroll-left 1))
  ;; (defun right-one () (interactive) (scroll-right 1))
  ;; (global-set-key (kbd "<mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<down-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<down-mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<double-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<double-mouse-5>") 'up-one)
  ;; (global-set-key (kbd "<double-down-mouse-4>") 'down-one)
  ;; (global-set-key (kbd "<double-down-mouse-5>") 'up-one)

  )
(provide 'lp-image)

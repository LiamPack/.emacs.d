                                        ; image manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;; C-c + / C-c -: Zoom in/out image.

;; C-c M-m: Adjust image to current frame size.

;; C-c C-x C-s: Save current image.

;; C-c M-r / C-c M-l: Rotate image.

;; C-c M-o: Show image image+ have not modified.



(use-package image+
  :ensure t
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

(provide 'use-image)

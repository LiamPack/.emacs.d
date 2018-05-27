(require 'use-package)

;; Ido configuration
(use-package flx-ido
  :disabled
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  (ido-ubiquitous-mode 1)
  (flx-ido-mode 1) ; better/faster matching
  (setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-completing-read+
  :ensure t)

(provide 'use-ido)

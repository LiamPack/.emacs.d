                                        ; c-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; also gdb is cool
(setq gdb-many-windows 't)

;; from https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
(use-package clang-format
  :defer t
  :straight t
  :bind (("C-c C-u" . clang-format-region)))

(use-package cc-mode
  :defer t
  :straight t
  :hook
  (c-common-mode-hook . hs-minor-mode)
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation))

;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :straight t
  :diminish cuda-mode
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(provide 'c-env)

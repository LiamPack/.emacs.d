                                        ; c-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; also gdb is cool

;; from https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
(use-package clang-format
  :defer t
  :straight t
  :bind (("C-c C-u" . clang-format-region)))

(use-package cc-mode
  :defer t
  :hook
  (c-common-mode-hook . hs-minor-mode)
  :init
  (setq gdb-many-windows 't)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'next-error)
  (setq compilation-skip-threshold 2)

  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq-default indent-tabs-mode nil))

(use-package cmake-mode
  :straight t)

;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :straight t
  :diminish cuda-mode
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(use-package eldoc-cmake
  :straight t
  :hook (cmake-mode-hook . eldoc-cmake-enable))

(provide 'lp-c-env)

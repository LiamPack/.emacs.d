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
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'next-error)

  (setq compilation-skip-threshold 2)
  (defcustom endless/compile-window-size 55
    "Width given to the non-compilation window."
    :type 'integer
    :group 'endless)

  (defun endless/compile-please (comint)
    "Compile without confirmation.
With a prefix argument, use comint-mode."
    (interactive "P")
    ;; Do the command without a prompt.
    (save-window-excursion
      (compile (eval compile-command) (and comint t)))
    ;; Create a compile window of the desired width.
    (pop-to-buffer (get-buffer "*compilation*"))
    (enlarge-window
     (- (frame-width)
        endless/compile-window-size
        (window-width))
     'horizontal))

  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq-default indent-tabs-mode nil)
  (add-hook 'c-mode-hook #'skeeto/c-hook)
  (add-to-list 'c-default-style '(c-mode . "k&r"))
  (add-to-list 'c-default-style '(c++-mode . "k&r"))
  (add-hook 'c-common-mode-hook (lambda ()
                                  (hs-minor-mode t)
                                  (local-set-key (kbd "C-c C-]") 'hs-show-block)
                                  (local-set-key (kbd "C-c C-_")  'hs-hide-block)
                                  (local-set-key (kbd "C-M-_")    'hs-hide-all)
                                  (local-set-key (kbd "C-M-]")  'hs-show-all)))
  (add-hook 'c++-mode-hook (lambda ()
                             (hs-minor-mode t)
                             (local-set-key (kbd "C-c C-]") 'hs-show-block)
                             (local-set-key (kbd "C-c C-_")  'hs-hide-block)
                             (local-set-key (kbd "C-M-_")    'hs-hide-all)
                             (local-set-key (kbd "C-M-]")  'hs-show-all)))
  (define-key c++-mode-map (kbd "C-c C-c") 'endless/compile-please)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (define-key c++-mode-map (kbd "C-c u") 'clang-format-buffer))

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

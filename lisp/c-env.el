                                        ; c-stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;;; A quick hook to C modes to quickswap to =.h= files or =.c= files. It's nice
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
;; also gdb is cool
(setq gdb-many-windows 't)




;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time

;; from https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9
(use-package clang-format
  :defer t
  :ensure t
  :bind (("C-c C-f" . clang-format-region)))


(use-package cc-mode
  :defer t
  :ensure t
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
  (use-package google-c-style
    :ensure t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    (add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode)))
  )

;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(provide 'c-env)

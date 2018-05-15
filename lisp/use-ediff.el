(require 'use-package)
(use-package ediff
     :config
     (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                   ediff-diff-options "-w")
     (add-hook 'ediff-prepare-buffer-hook
               (lambda ()
                 (when (derived-mode-p 'outline-mode)
                   (outline-show-all)))))

(provide 'use-ediff)

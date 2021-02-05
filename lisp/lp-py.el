(require 'use-package)

(use-package python
  :straight t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (custom-set-variables
   '(flycheck-python-flake8-executable "python3")
   '(flycheck-python-pycompile-executable "python3")
   '(flycheck-python-pylint-executable "python3"))
  )

(use-package pyvenv
  :straight t)

(use-package yapfify
  :straight t
  :hook (python-mode-hook . yapf-mode))

(use-package julia-mode
  :straight t)

(provide 'lp-py)

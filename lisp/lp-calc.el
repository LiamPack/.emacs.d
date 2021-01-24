(use-package calc
  :straight t
  :bind ("C-c =" . calc)
  :config (setf calc-display-trail nil))

(provide 'lp-calc)

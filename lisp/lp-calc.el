
(require 'use-package)

(use-package calc
  :bind ("C-c =" . calc)
  :config (setf calc-display-trail nil))

(provide 'lp-calc)

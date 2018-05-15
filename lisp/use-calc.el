(require 'use-package)
(use-package calc
  :ensure t
  :bind ("C-c =" . calc)
  :config (setf calc-display-trail nil))

(provide 'use-calc)

(require 'use-package)
(use-package ess
     :ensure t
     :config
     (setq-default inferior-S+6-program-name "Splus")
     (setq-default inferior-R-program-name "R"))

(provide 'use-ess)

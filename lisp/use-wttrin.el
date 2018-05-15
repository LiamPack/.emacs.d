                                        ; weather
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(use-package wttrin
  :defer t
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Swarthmore")))
(provide 'use-wttrin)

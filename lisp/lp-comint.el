(require 'use-package)
(use-package comint
  :init
  (setf comint-prompt-read-only t
        comint-history-isearch t))
(provide 'lp-comint)

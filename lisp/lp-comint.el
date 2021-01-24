(require 'use-package)
(require 'comint)

(setf comint-prompt-read-only t
      comint-history-isearch t)

(provide 'lp-comint)

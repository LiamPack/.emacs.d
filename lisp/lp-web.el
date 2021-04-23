
                                        ; web dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

 (use-package rjsx-mode
   :straight t
   :mode "\\.\\(js\\|jsx\\)\\'")

 (use-package typescript-mode
   :straight t
   :mode "\\.ts\\'")

 (use-package prettier
   :straight t)

(provide 'lp-web)

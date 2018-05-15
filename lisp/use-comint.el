(require 'use-package)
(require 'comint)

(define-key comint-mode-map (kbd "<down>") #'comint-next-input)
(define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-n") #'comint-next-input)
(define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
(setf comint-prompt-read-only t
      comint-history-isearch t)

(provide 'use-comint)

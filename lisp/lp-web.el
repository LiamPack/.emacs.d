;;; lp-web.el -- configuration for web development (js, ts, jsx, etc)
;;; Commentary:
(require 'use-package)

;;; Code:

(use-package rjsx-mode
  :straight t
  :mode "\\.\\(js\\|jsx\\)\\'")

(use-package typescript-mode
  :straight t)


(use-package web-mode
  :straight t
  :after typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package prettier
  :straight t
  :hook ((js2-mode-hook web-mode-hook rjsx-mode-hook) . prettier-js-mode)
  :config
  (defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.jsx?\\'" . prettier-js-mode))))
  (add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.tsx?\\'" . prettier-js-mode))))
  )

(provide 'lp-web)
;;; lp-web.el ends here


                                        ; web dev
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(use-package web-mode
  :ensure t
  :diminish web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.phtml\\'"      . web-mode)
         ("\\.tpl\\.php\\'"  . web-mode)
         ("\\.jsp\\'"        . web-mode)
         ("\\.as[cp]x\\'"    . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.djhtml\\'"     . web-mode)
         ("\\.html?\\'"      . web-mode)
         ("\\.hbs\\'"        . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (define-key web-mode-map (kbd "<backtab>") 'web-mode-fold-or-unfold)

  ;; ;; Make sure that these lovely templating languages use web-mode
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  ;; (add-to-list 'auto-mode-alist ')
  )

(use-package emmet-mode
  :ensure t
  :mode ("\\.html" . emmet-mode)
  :diminish emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (setq emmet-move-cursor-between-quotes t))

(use-package impatient-mode ; auto-update browser without having to
                            ; reload when editing web stuff
  :defer t
  :ensure t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(use-package simple-httpd ; httpd stuff
  :ensure t
  :defer t
  :functions httpd-send-header
  :config
  (progn
    (defservlet uptime "text/plain" ()
      (princ (emacs-uptime)))
    (defun httpd-here ()
      (interactive)
      (setf httpd-root default-directory))
    (defadvice httpd-start (after httpd-query-on-exit-flag activate)
      (let ((httpd-process (get-process "httpd")))
        (when httpd-process
          (set-process-query-on-exit-flag httpd-process nil))))))

(use-package js2-mode ; javascript editing 
  :ensure t
  :diminish (js-mode . "js")
  :mode "\\.js$"
  :config
  (progn
    (use-package js2-refactor
      :ensure t)
    (use-package xref-js2
      :ensure t)

    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook (lambda () (progn
                                     (setq mode-name "js2")
                                     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))


    (setf js2-skip-preprocessor-directives t)
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "skewer"
                    "console" "phantom"))))


;; more info here https://github.com/skeeto/skewer-mode
;; check it out https://github.com/smihica/emmet-mode
;; C-x C-e: Evaluate the form before the point and display the result in the minibuffer. If given a prefix argument, insert the result into the current buffer.
;; C-M-x: Evaluate the top-level form around the point.
;; C-c C-k: Load the current buffer.
;; C-c C-z: Select the REPL buffer.
(use-package skewer-mode
  :ensure t
  :diminish (skewer-mode . "sk")
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package request
  :ensure t)

(provide 'use-web)

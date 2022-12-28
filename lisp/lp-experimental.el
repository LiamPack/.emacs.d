(lp-emacs-elpa-package 'simple-httpd)
(lp-emacs-elpa-package 'f)
(lp-emacs-elpa-package 's)
(lp-emacs-elpa-package 'dash)
(lp-emacs-elpa-package 'async)

(lp-emacs-elpa-package 'tree-sitter)
(lp-emacs-elpa-package 'tree-sitter-langs)
(lp-emacs-elpa-package 'org-ql) ;; for updating note molds via org export

(lp-emacs-git-package
    'moldable-emacs "https://github.com/ag91/moldable-emacs.git"
    (me-setup-molds)
    (let ((map global-map))
      (define-key map (kbd "C-c m m") #'me-mold)
      (define-key map (kbd "C-c m f") #'me-go-forward)
      (define-key map (kbd "C-c m b") #'me-go-back)
      (define-key map (kbd "C-c m o") #'me-open-at-point)
      (define-key map (kbd "C-c m d") #'me-docs)
      (define-key map (kbd "C-c m g") #'me-goto-mold-source)
      (define-key map (kbd "C-c m e a") #'me-mold-add-last-example)))

(provide 'lp-experimental)

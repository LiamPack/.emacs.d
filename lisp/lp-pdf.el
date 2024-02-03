(lp-emacs-elpa-package 'pdf-tools
  (setq pdf-tools-enabled-modes
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-occur-global-minor-mode))

  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-continuous t)
  (setq pdf-view-use-dedicated-register nil)
  (setq pdf-view-max-image-width 2160)
  (setq pdf-outline-imenu-use-flat-menus t)

  (pdf-loader-install)

  (add-hook 'pdf-view-mode-hook #'pdf-view-themed-minor-mode))

(provide 'lp-pdf)

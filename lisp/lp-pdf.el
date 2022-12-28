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

  (with-eval-after-load 'modus-themes

    ;;; credits to prot as always!
    ;; Those functions and hooks are adapted from the manual of my modus-themes.
    ;; The idea is to (i) add a backdrop that is distinct from the background of
    ;; the PDF's page and (ii) make pdf-tools adapt to theme switching via, e.g.,
    ;; `modus-themes-toggle'.
    (defun prot/pdf-tools-backdrop ()
      (face-remap-add-relative
       'default `(:background
                  ,(modus-themes-color
                    'bg-alt))))

    (defun prot/pdf-tools-midnight-mode-toggle ()
      (when (derived-mode-p 'pdf-view-mode)
        (if (or (eq (car (custom-enabled-themes)) 'standard-dark)
                (eq (car custom-enabled-themes) 'modus-vivendi))
            (pdf-view-midnight-minor-mode 1)
          (pdf-view-midnight-minor-mode -1))
        (prot/pdf-tools-backdrop)))

    (add-hook 'pdf-tools-enabled-hook #'prot/pdf-tools-midnight-mode-toggle)
    (add-hook 'modus-themes-after-load-theme-hook #'prot/pdf-tools-midnight-mode-toggle)))

(provide 'lp-pdf)

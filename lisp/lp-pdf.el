;;; TODO: personalize. This could be the most important part of the
;;; config.  (scrolling, bookmarking and marking, seamless transition
;;; between pdfs).
(lp-emacs-elpa-package 'pdf-tools
  (setq pdf-tools-enabled-modes
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-occur-global-minor-mode
	  pdf-view-themed-minor-mode
	  pdf-annot-minor-mode
	  pdf-sync-minor-mode
	  pdf-view-roll-minor-mode))
  ;; view-roll minor mode...
  (setq pdf-view-display-size 'fit-width)
  (setq pdf-view-continuous t)
  (setq pdf-view-use-dedicated-register t)
  (setq pdf-view-max-image-width 2160)
  (setq pdf-outline-imenu-use-flat-menus t)

  (pdf-loader-install))


;; (lp-emacs-elpa-package 'djvu
;;   (lp-emacs-elpa-package 'svg)
;;   (lp-emacs-git-package 'djvu3
;;     "https://github.com/dalanicolai/djvu3.git"))


;; Roll mode can mistake the current-line highlight overlay for a page
;; overlay.  Exclude PDF buffers from highlighting before redisplay.
(defun lp-pdf-disable-line-highlighting ()
  (setq-local global-hl-line-buffers nil)
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1)))

(add-hook 'pdf-view-mode-hook #'lp-pdf-disable-line-highlighting)

(provide 'lp-pdf)

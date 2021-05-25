
                                        ; writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
(use-package olivetti
  :straight t)

(use-package visual-fill-column
  :straight t)
;; Typical .dir-local.el for writing stuff:
;; 
;; ((org-mode . ((eval . (progn (turn-off-auto-fill)
;;                              (text-scale-set 1)
;;                              (olivetti-mode)))
;;               (fill-column              . 80)
;;               (visual-fill-column-width . 80)
;;               (olivetti-body-width      . 80)
;;               (mode . visual-line)
;;               (mode . visual-fill-column))))

(provide 'lp-writing)

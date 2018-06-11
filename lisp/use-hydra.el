(require 'use-package)
(use-package hydra
  :ensure t
  :config
  (progn
    (defhydra hydra-yank-pop ()
      "yank"
      ("C-y" yank nil)
      ("M-y" yank-pop nil)
      ("y" (yank-pop 1) "next")
      ("Y" (yank-pop -1) "prev")
      ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
    (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
    (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)
    (defhydra hydra-hs (:idle 1.0)
      "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
      ("s" hs-show-all)
      ("h" hs-hide-all)
      ("a" hs-show-block)
      ("d" hs-hide-block)
      ("t" hs-toggle-hiding)
      ("l" hs-hide-level)
      ("n" forward-line)
      ("p" (forward-line -1))
      ("SPC" nil))
    (global-set-key (kbd "C-c @") 'hydra-hs/body)
    ))



(provide 'use-hyrda)

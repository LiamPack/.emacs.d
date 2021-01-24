(require 'use-package)

;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
;; shamelessly taken from this thread
(use-package erc
  :disabled t
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "#bash" "#bitcoin"
                                  "#emacs" "#gentoo" "#i3" "#latex" "#org-mode" "#python")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-prompt-for-nickserv-password nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
  (erc-lurker-threshold-time 43200)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules))

;; TODO set auth sources, some other helper functions to do things..

(use-package erc-hl-nicks ; coloring pseudonyms
  :after erc)

(provide 'use-erc)

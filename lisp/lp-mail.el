(use-package mm-encode
  :ensure nil
  :defer t
  :config
  (setq mm-encrypt-option nil ; use 'guided for both if you need more control
        mm-sign-option nil))

(use-package mml-sec
  :ensure nil
  :defer t
  :config
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t))

;;;; Message composition (`message')

(use-package message
  :ensure nil
  :defer t
  :hook
  (message-setup . message-sort-headers)
  :config
  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t) ; use `mail-user-agent'
  (setq mail-header-separator "--text follows this line--")
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings t)
  ;; (setq message-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n"
  ;;       mail-signature message-signature)
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers "") ; default is "." for all headers
  (setq message-confirm-send t)
  (setq message-kill-buffer-on-exit t)
  ;; (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))
  (setq message-wide-reply-confirm-recipients t))

;; (use-package sendmail
;;   :ensure nil
;;   :after message
;;   :config
;;   (setq send-mail-function 'sendmail-send-it
;;         ;; ;; NOTE 2023-08-08: We do not need this if we have the Arch
;;         ;; ;; Linux `msmtp-mta' package installed: it replaces the
;;         ;; ;; generic sendmail executable with msmtp.
;;         ;;
;;         ;; sendmail-program (executable-find "msmtp")
;;         message-sendmail-envelope-from 'header))



(provide 'lp-mail)

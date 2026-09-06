
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

(use-package sendmail
  :ensure nil
  :after message
  :config
  (setq sendmail-program (or (executable-find "msmtp")
                           "/usr/bin/msmtp")
      send-mail-function #'sendmail-send-it
      message-send-mail-function #'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header))


(lp-emacs-elpa-package 'notmuch
  (setq mail-user-agent 'notmuch-user-agent
	user-full-name "Liam Packer"
	user-mail-address "lp492@cornell.edu"

	notmuch-identities
	'("Liam Packer <lp492@cornell.edu>")

	notmuch-show-logo nil
        notmuch-hello-auto-refresh t
        notmuch-hello-sections
        '(notmuch-hello-insert-saved-searches)

        notmuch-search-oldest-first nil
        notmuch-show-relative-dates t
        notmuch-show-mark-read-tags '("-unread")

        ;; Gmail remains authoritative for archiving.
        notmuch-archive-tags nil

        ;; Privacy and presentation.
        notmuch-show-text/html-blocked-images "."
        notmuch-show-all-multipart/alternative-parts nil
        notmuch-wash-wrap-lines-length 100


	;; Gmail's SMTP service places sent messages in Sent Mail.
	;; mbsync will subsequently retrieve them.
	notmuch-fcc-dirs nil

	notmuch-show-logo nil

	notmuch-saved-searches
	`((:name "inbox"
		 :query "tag:inbox"
		 :key ,(kbd "i"))
          (:name "unread"
		 :query "tag:inbox and tag:unread"
		 :key ,(kbd "u"))
          (:name "flagged"
		 :query "tag:flagged"
		 :key ,(kbd "f"))
          (:name "sent"
		 :query "tag:sent"
		 :key ,(kbd "t"))
          (:name "drafts"
		 :query "tag:draft"
		 :key ,(kbd "d"))
          (:name "recent"
		 :query "date:2weeks.."
		 :key ,(kbd "r"))))
  (add-hook 'notmuch-mua-send-hook
            #'notmuch-mua-attachment-check)
  )

;; ;;; Send through msmtp

;; (setq sendmail-program "/usr/bin/msmtp"
;;       send-mail-function #'sendmail-send-it
;;       message-send-mail-function #'message-send-mail-with-sendmail
;;       message-sendmail-extra-arguments '("--read-envelope-from")
;;       message-sendmail-f-is-evil t
;;       message-kill-buffer-on-exit t)

;; (use-package mu4e
;;   :ensure nil
;;   :load-path "/usr/share/emacs/site-lisp/mu4e/"
;;   :commands (mu4e mu4e-compose-new)
;;   :bind (("C-c m" . mu4e)
;;          ("C-x m" . mu4e-compose-new))
;;   :init
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   :config
;;   ;; Identity
;;   (setq user-full-name "Liam Packer"
;;         user-mail-address "lp492@cornell.edu")

;;   ;; This must match the directory passed to `mu init'.
;;   (setq mu4e-maildir (expand-file-name "~/Maildir/cornell"))

;;   ;; Let Mu4e invoke mbsync and then update the mu index.
;;   (setq mu4e-get-mail-command
;;         "mbsync -c /home/lim/.config/isyncrc cornell"
;;         mu4e-update-interval 300
;;         mu4e-index-lazy-check t)

;;   ;; Cornell Gmail folders. Adjust these if your local folder names differ.
;;   (setq mu4e-inbox-folder  "/INBOX"
;;         mu4e-sent-folder   "/[Gmail]/Sent Mail"
;;         mu4e-drafts-folder "/[Gmail]/Drafts"
;;         mu4e-trash-folder  "/[Gmail]/Trash"
;;         mu4e-refile-folder "/[Gmail]/All Mail")

;;   ;; Gmail records SMTP submissions in Sent automatically. Do not create
;;   ;; another local sent copy.
;;   (setq mu4e-sent-messages-behavior 'delete)

;;   ;; Useful with Maildir synchronizers.
;;   (setq mu4e-change-filenames-when-moving t
;;         mu4e-headers-skip-duplicates t
;;         mu4e-headers-include-related nil)

;;   ;; General presentation and composition.
;;   (setq mu4e-attachment-dir (expand-file-name "~/Downloads")
;;         mu4e-confirm-quit nil
;;         mu4e-view-show-addresses t
;;         mu4e-view-show-images nil
;;         mu4e-compose-complete-only-personal t
;;         message-kill-buffer-on-exit t)

;;   ;; Folder shortcuts, available through `j'.
;;   (setq mu4e-maildir-shortcuts
;;         '((:maildir "/INBOX"
;;            :key ?i
;;            :favorite t)
;;           (:maildir "/[Gmail]/All Mail"
;;            :key ?a)
;;           (:maildir "/[Gmail]/Sent Mail"
;;            :key ?s)
;;           (:maildir "/[Gmail]/Drafts"
;;            :key ?d)
;;           (:maildir "/[Gmail]/Trash"
;;            :key ?t)))

;;   ;; Prot-inspired focused searches.
;;   (setq mu4e-bookmarks
;;         '((:name "Unread inbox"
;;            :query "maildir:/INBOX AND flag:unread"
;;            :key ?u
;;            :favorite t)
;;           (:name "Inbox"
;;            :query "maildir:/INBOX"
;;            :key ?i)
;;           (:name "Today"
;;            :query "date:today..now"
;;            :key ?n)
;;           (:name "Last seven days"
;;            :query "date:7d..now"
;;            :key ?w)
;;           (:name "Flagged"
;;            :query "flag:flagged"
;;            :key ?f)
;;           (:name "Attachments"
;;            :query "flag:attach"
;;            :key ?A))))



(provide 'lp-mail)

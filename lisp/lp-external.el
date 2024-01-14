;;; email
(lp-emacs-elpa-package 'notmuch
  (setq notmuch-show-logo nil)
  (setq notmuch-column-control t)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator "")
  ;; ;; See my variant of it in `prot-notmuch' below.
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (setq notmuch-show-all-tags-list t)

  (setq notmuch-search-oldest-first nil)

  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)

  (setq notmuch-saved-searches
        `(( :name "all"
            :query "not tag:archived and not tag:list and not tag:lists and not tag:spam"
            :sort-order newest-first
            :key ,(kbd "a"))
          ( :name "inbox"
            :query "tag:inbox not tag:archived"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "unread all"
            :query "tag:unread and not tag:archived and not tag:list and not tag:lists"
            :sort-order newest-first
            :key ,(kbd "U"))
          ( :name "personal"
            :query "tag:personal not tag:archived"
            :sort-order newest-first
            :key ,(kbd "p"))
          ( :name "personal all"
            :query "tag:personal"
            :sort-order newest-first
            :key ,(kbd "P"))
          ( :name "todo"
            :query "tag:todo not tag:archived"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "todo all"
            :query "tag:todo"
            :sort-order newest-first
            :key ,(kbd "T"))
          ( :name "done"
            :query "tag:done"
            :sort-order newest-first
            :key ,(kbd "d"))
          ( :name "flagged"
            :query "tag:flag or tag:flagged or tag:important or tag:starred"
            :sort-order newest-first
            :key ,(kbd "f"))
          ( :name "mailing lists"
            :query "tag:list or tag:lists not tag:archived"
            :key ,(kbd "m"))
          ))

  (setq notmuch-archive-tags '("-inbox" "+archived"))
  (setq notmuch-message-replied-tags '("+replied"))
  (setq notmuch-message-forwarded-tags '("+forwarded"))
  (setq notmuch-show-mark-read-tags '("-unread"))
  (setq notmuch-draft-tags '("+draft"))
  (setq notmuch-draft-folder "drafts")
  (setq notmuch-draft-save-plaintext 'ask)

  ;; ;; NOTE 2021-06-18: See an updated version in the `prot-notmuch'
  ;; ;; section below.
  (setq notmuch-tagging-keys
        `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
          (,(kbd "c") ("+archived" "-inbox" "-list" "-todo" "-ref" "-unread" "+done") "Complete and archive")
          (,(kbd "d") ("+del" "-inbox" "-archived" "-unread") "Mark for deletion")
          (,(kbd "f") ("+flag" "-unread") "Flag as important")
          ;; (,(kbd "r") notmuch-show-mark-read-tags "Mark as read")
          (,(kbd "r") ("+ref" "+notes" "-unread") "Reference for the future")
          (,(kbd "s") ("+spam" "+del" "-inbox" "-unread") "Mark as spam")
          (,(kbd "t") ("+todo" "-unread") "To-do")
          (,(kbd "u") ("+unread") "Mark as unread")))

  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;;; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil) ; TODO 2021-05-12: Review hidden headers
  (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\)"
                "\\b"))

  ;;; Reading messages
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-save-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-citation-lines-prefix 6)
  (setq notmuch-wash-citation-lines-suffix 6)
  (setq notmuch-wash-wrap-lines-length 100)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  ;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  ;; Use alternating backgrounds, if `stripes' is available.
  (with-eval-after-load 'stripes
    (add-hook 'notmuch-search-hook #'stripes-mode)
    ;; ;; To disable `hl-line-mode':
    ;; (setq notmuch-search-hook nil)
    ;; (add-hook 'notmuch-search-hook #'prot-common-disable-hl-line)
    )

  (let ((map global-map))
    ;; (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (define-key notmuch-search-mode-map (kbd "/") #'notmuch-search-filter) ; alias for l
  (define-key notmuch-hello-mode-map (kbd "C-<tab>") nil)
  )


  ;;; Sending email (SMTP)
(lp-emacs-builtin-package 'smtpmail
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-queue-mail nil)
  ;; (setq smtpmail-smtp-server "smtp.gmail.com")
  ;; (setq smtpmail-auth-credentials '(("smtp.gmail.com" 465 "liampacker@gmail.com" "acydhqtbgeudyvxm")))
  (setq smtpmail-debug-info t)
  (setq smtpmail-debug-verb t)
  )

(lp-emacs-builtin-package 'sendmail
  (setq send-mail-function 'smtpmail-send-it))

;;; internet browsing
(lp-emacs-builtin-package 'eww
  (define-key global-map (kbd "<f12>") 'eww)
  ;; (define-key global-map (kbd "M-(") 'eww)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width nil)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil)

  (setq eww-search-prefix "https://www.duckduckgo.com/?q=")

  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser)
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  (setq eww-retrieve-command nil)

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill) ; same
  )

;;; RSS feed
(lp-emacs-elpa-package 'elfeed
  (define-key global-map (kbd "C-c e") #'elfeed)

  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 60)
  (setq elfeed-search-title-min-width 40)
  (setq elfeed-search-trailing-width 70)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank))

  ;; This is set in two places now, once in =eww= and once here.
  ;; (setq browse-url-browser-function 'eww-browse-url)
  ;; (setq browse-url-secondary-browser-function 'browse-url-default-browser)
  (setq lp--elfeed-tags '(star stats leftist programming philosophy emacs academic pl writing lisp dead))
  ;;; Elfeed feeds
  ;; https://leahneukirchen.org/MySubscriptions.cgi
  (setq elfeed-feeds '("https://freddiedeboer.substack.com/feed.rss" ;; classical marxist
                       ("https://www.juliabloggers.com/feed/" programming) ;; julia blogs
                       ("https://protesilaos.com/master.xml" star philosophy emacs)
                       ;; "http://rachelbythebay.com/w/atom.xml"
                       ("https://ava.substack.com/feed" writing)
                       ("https://askmolly.substack.com/feed" writing)
                       ("https://askpolly.substack.com/feed" writing)
                       ("https://cherylstrayed.substack.com/feed" writing)
                       ("https://griefbacon.substack.com/feed" writing)
                       ("https://defaultfriend.substack.com/feed" writing)
		       ("https://internetprincess.substack.com/feed" writing)
                       ("https://graymirror.substack.com/feed" leftist)
                       ("https://jdahl.substack.com/feed" writing)
                       ("http://inconvergent.net/atom.xml" art lisp)
                       "http://nullprogram.com/feed/"
                       ("http://feeds.feedburner.com/datacolada/" stats)
                       ("https://tymoon.eu/api/reader/atom" lisp gamedev) ;; kandria dev
                       ("https://alhassy.github.io/rss.xml" lisp) ;; lisper, had that great post about emacs lisp's type system
		       ("https://borretti.me/feed.xml" lisp writing) ;; guy who did the astro+lisp post 
                       "https://gwern.substack.com/feed"
                       "https://www.benkuhn.net/index.xml" ;; harvard xd
                       ("https://www.nayuki.io/rss20.xml" programming) ;; swe in canada with classic math posts
                       ("https://blog.nelhage.com/atom.xml" programming swe) ;; guy who made the networks classic guy
                       ("https://lacker.io/feed.xml" data-science)
                       ("https://danluu.com/atom.xml" star stats)
		       ("https://statisticaloddsandends.wordpress.com/atom.xml" stats)
		       ("https://mathematicaloddsandends.wordpress.com/atom.xml" math)
		       ("http://www.math3ma.com/blog/rss.xml" math)
		       ("https://golem.ph.utexas.edu/category/rss.html" math)
		       ("https://johncarlosbaez.wordpress.com/atom.xml" math physics)
		       ;; "http://bactra.org/weblog/index.rss"
                       ;; "http://bactra.org/notebooks/index.rss" ;; cmu guy
		       ;; "https://prospect.org/api/rss/content_full.rss"
		       "https://leahneukirchen.org/trivium/index.atom"
                       ;; ("http://understandinguncertainty.org/blog" dead) ;; dead shit
                       )))

;;; epub reader
(lp-emacs-elpa-package 'nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (setq nov-text-width nil)
  (add-hook 'nov-mode-hook 'visual-line-mode)

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.0))

  (add-hook 'nov-mode-hook 'my-nov-font-setup))

;;; music player
(lp-emacs-elpa-package 'bongo
  (setq bongo-default-directory "/home/liamp/Desktop/musics/")
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress nil) ; t slows down the playlist buffer
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (bongo-mode-line-indicator-mode -1)
  (bongo-header-line-mode -1)
  (let ((map global-map))
    (define-key map (kbd "C-c b") #'bongo)
    (define-key map (kbd "<C-XF86AudioPlay>") #'bongo-pause/resume)
    (define-key map (kbd "<C-XF86AudioNext>") #'bongo-next)
    (define-key map (kbd "<C-XF86AudioPrev>") #'bongo-previous)
    (define-key map (kbd "<C-M-XF86AudioPlay>") #'bongo-play-random)
    (define-key map (kbd "<M-XF86AudioPlay>") #'bongo-show)
    (define-key map (kbd "<S-XF86AudioNext>") #'bongo-seek-forward-10)
    (define-key map (kbd "<S-XF86AudioPrev>") #'bongo-seek-backward-10))
  (let ((map bongo-playlist-mode-map))
    (define-key map (kbd "n") #'bongo-next-object)
    (define-key map (kbd "p") #'bongo-previous-object)
    (define-key map (kbd "R") #'bongo-rename-line)
    (define-key map (kbd "j") #'bongo-dired-line)       ; Jump to dir of file at point
    (define-key map (kbd "J") #'dired-jump)             ; Jump to library buffer
    (define-key map (kbd "I") #'bongo-insert-special)))

(provide 'lp-external)

;;; internet browsing
(lp-emacs-builtin-package 'eww
  (define-key global-map (kbd "<f12>") 'eww)
  ;; (define-key global-map (kbd "M-(") 'eww)
  (setq shr-use-colors t)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)
  (setq shr-width nil)
  (setq shr-discard-aria-hidden nil)
  (setq shr-cookie-policy nil)

  (setq eww-search-prefix "https://www.duckduckgo.com/?q=")

  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser)
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%t: %u")
  (setq eww-download-directory (locate-user-emacs-file "eww-downloads/"))
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
  ;; (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill) ; same
  )

;;; RSS feed
(lp-emacs-elpa-package 'elfeed
  (define-key global-map (kbd "C-c e") #'elfeed)

  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory  "~/dropbox/denotes/.elfeed/")
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 60)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank))

  (setq lp--elfeed-tags '(critical important personal))

  ;;; Elfeed feeds
  ;; TODO: update.
  (setq elfeed-feeds '("https://protesilaos.com/master.xml"
                       "https://ava.substack.com/feed"
		       "https://proustproject.substack.com/feed"
		       "https://pahlkadot.substack.com/feed"
		       "https://catalyst-journal.com/feed"
		       "https://tetw.org/rss"

		       "http://export.arxiv.org/api/query?search_query=cat:math.AP&start=0&max_results=150&sortBy=submittedDate&sortOrder=descending"
		       "http://export.arxiv.org/api/query?search_query=cat:math.PR&start=0&max_results=150&sortBy=submittedDate&sortOrder=descending"
		       "http://export.arxiv.org/api/query?search_query=cat:math.MP&start=0&max_results=150&sortBy=submittedDate&sortOrder=descending"
		       "https://djalil.chafai.net/blog/rss" ; hardcore french probability
		       "https://mbounthavong.com/blog/rss.xml" ; clinical pharmacology prof at ucsd
		       "https://scottaaronson.blog/rss"

		       "https://terrytao.wordpress.com/feed/"
		       "https://johncarlosbaez.wordpress.com/atom.xml"

		       "https://borretti.me/feed.xml" ; did the astro+lisp post 
                       "http://inconvergent.net/atom.xml" ; lisp art
                       "http://nullprogram.com/feed/" ; apl
		       "https://leahneukirchen.org/trivium/index.atom" ; killer hacker

                       "https://gwern.substack.com/feed"
                       "https://www.benkuhn.net/index.xml" ; harvard xd
                       "https://www.nayuki.io/rss20.xml" ; swe in canada, detailed
                       "https://danluu.com/atom.xml" ; plain but good

		       "https://feeds.acast.com/public/shows/58ad887a1608b1752663b04a"
                       ))

  ;; Ripping all arxiv-related feed stuff from
  ;; https://cundy.me/post/elfeed/
  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
    (mapconcat
     (lambda (author) (plist-get author :name))
     authors-list ", "))
  (defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
	   (title (or (elfeed-meta entry :title)
		      (elfeed-entry-title entry) ""))
	   (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	   (feed (elfeed-entry-feed entry))
	   (feed-title
	    (when feed
	      (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
	   (entry-authors (concatenate-authors
			   (elfeed-meta entry :authors)))
	   (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	   (tags-str (mapconcat
		      (lambda (s) (propertize s 'face
					      'elfeed-search-tag-face))
		      tags ","))
	   (title-width (- (window-width) 10
			   elfeed-search-trailing-width))
	   (title-column (elfeed-format-column
			  title (elfeed-clamp
				 elfeed-search-title-min-width
				 title-width
				 elfeed-search-title-max-width)
			  :left))
	   (authors-width 40)
	   (authors-column (elfeed-format-column
			    entry-authors (elfeed-clamp
					   elfeed-search-title-min-width
					   authors-width
					   40)
			    :left)))

      (insert (propertize date 'face 'elfeed-search-date-face) " ")

      (insert (propertize title-column
			  'face title-faces 'kbd-help title) " ")

      (insert (propertize authors-column
			  'face 'elfeed-search-date-face
			  'kbd-help entry-authors) " ")

      ;; (when feed-title
      ;;   (insert (propertize entry-authors
      ;; 'face 'elfeed-search-feed-face) " "))

      (when entry-authors
	(insert (propertize feed-title
			    'face 'elfeed-search-feed-face) " "))

      ;; (when tags
      ;;   (insert "(" tags-str ")"))

      )
    )
  (setq elfeed-search-print-entry-function #'my-search-print-fn)
  )
(lp-emacs-elpa-package 'elfeed-score
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map (kbd "=") elfeed-score-map)
  (setq elfeed-score-score-file (concat user-emacs-directory "elfeed.score"))
  )

;;; epub reader
;;; TODO: i don't like this
(lp-emacs-elpa-package 'nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (setq nov-text-width nil)
  (add-hook 'nov-mode-hook 'visual-line-mode)

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.0))

  (add-hook 'nov-mode-hook 'my-nov-font-setup))

;;; music player
;;; TODO: this isn't personalized
(lp-emacs-elpa-package 'bongo
  ;; (setq bongo-default-directory "/media/lim/X1/")
  (setq bongo-default-directory "/home/lim/dropbox/music/")  
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


;;; TODO: what can i do with this
(lp-emacs-elpa-package 'arxiv-mode
  (setq arxiv-pop-up-new-frame nil)
  (setq arxiv-startup-with-abstract-window t)
  (setq arxiv-default-category "math.pr")
  )

(setq load-path (append (list (expand-file-name "~/.emacs.d/local/")) load-path)) 
(autoload 'LilyPond-mode "lilypond-mode")

(provide 'lp-external)

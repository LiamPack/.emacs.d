                                        ; w3m and internet: TODO change this to eww
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; TAB to jump from link to link.
;; RETURN to follow a link
;; SPACE to move down the page
;; b to move up the page
;; B to move back in the history
;; M to open the URL in Firefox
;; I to open the image if it didn’t show up correctly
;; c to copy the URL of the current page in the kill ring.
;; u to copy the URL of the link in the kill ring.
;; a to bookmark this page
;; v to look at the bookmarks
;; s to look through the page history for this session.
(use-package w3m
  :defer t
  :straight t
  :commands w3m-goto-url w3m-search
  :init
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-use-cookies t)

  ;; clean up the w3m buffers:
  (add-hook 'w3m-display-functions 'w3m-hide-stuff)
  (add-hook 'w3m-mode 'ace-link-mode)

  (global-set-key (kbd "C-c w w") 'w3m-goto-url)
  (global-set-key (kbd "C-c w l") 'browse-url-at-point)
  (global-set-key (kbd "C-c w g") 'w3m-search)

  :config
  (define-key w3m-mode-map (kbd "&") 'w3m-view-url-with-external-browser))

(defun ha-switch-default-browser ()
  "Switches the default browser between the internal and external web browser."
  (interactive)
  ;;         | Variable                  | Function
  (if (equal browse-url-browser-function 'browse-url-default-browser)
      (if (fboundp 'w3m)
          (setq browse-url-browser-function 'w3m-browse-url)
        (setq browse-url-browser-function 'eww-browse-url))
    (setq browse-url-browser-function 'browse-url-default-browser))

  ;; Now we need to display the current setting. The variables are
  ;; pretty typical and have the goodies, but I just need to get rid
  ;; of the word "url" or "browser", and the results are pretty close:
  (cl-flet ((remove-bad-parts (l)
                              (-filter (lambda (s) (pcase s
                                                     ("url"     nil)
                                                     ("browse"  nil)
                                                     ("browser" nil)
                                                     (_  t))) l)))
    (message "Browser set to: %s"
             (-> (symbol-name browse-url-browser-function)
                 (split-string "-")
                 remove-bad-parts
                 car))))

(global-set-key (kbd "C-c w d") 'ha-switch-default-browser)

(defun w3m-skip-in-google ()
  "For a Google Search, skip to the first result."
  (beginning-of-buffer)
  (search-forward-regexp "[0-9, ]+ results")
  (forward-line 2)
  (recenter-top-bottom 0))

(provide 'use-w3m)

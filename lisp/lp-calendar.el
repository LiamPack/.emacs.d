(lp-emacs-builtin-package 'sendmail
 (setq send-mail-function 'sendmail-send-it
        ;; ;; NOTE 2023-08-08: We do not need this if we have the Arch
        ;; ;; Linux `msmtp-mta' package installed: it replaces the
        ;; ;; generic sendmail executable with msmtp.
        ;;
        ;; sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header))

(lp-emacs-builtin-package 'calendar
  ;; lots ripped from prot
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (require 'solar)
  (setq calendar-latitude 39.0         ; Not my actual coordinates
        calendar-longitude -76.4)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "EST")
  (setq calendar-daylight-time-zone-name "EDT")

  (require 'diary-lib)
  (setq diary-file (file-truename "~/dropbox/denotes/diary"))
  (setq user-mail-address "liamp@TheCave")
  (setq diary-mail-addr user-mail-address)
  (setq diary-date-forms diary-iso-date-forms)
  (setq diary-comment-start ";;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")
  (setq diary-show-holidays-flag t)
  (setq diary-display-function #'diary-fancy-display) ; better than its alternative
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-number-of-entries 3)
  (setq diary-mail-days 3)
  (setq diary-abbreviated-year-flag nil)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'diary-mode-hook #'goto-address-mode) ; buttonise plain text links

  ;; Those presuppose (setq diary-display-function #'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  ;; Prevent Org from interfering with my key bindings.
  (remove-hook 'calendar-mode-hook #'org--setup-calendar-bindings)

  (let ((map calendar-mode-map))
    (define-key map (kbd "s") #'calendar-sunrise-sunset)
    (define-key map (kbd "l") #'lunar-phases)
    (define-key map (kbd "i") nil) ; Org sets this, much to my chagrin (see `remove-hook' above)
    (define-key map (kbd "i a") #'diary-insert-anniversary-entry)
    (define-key map (kbd "i b") #'diary-insert-block-entry)
    (define-key map (kbd "i c") #'diary-insert-cyclic-entry)
    (define-key map (kbd "i d") #'diary-insert-entry) ; for current "day"
    (define-key map (kbd "i i") #'diary-insert-entry) ; most common action, easier to type
    (define-key map (kbd "i m") #'diary-insert-monthly-entry)
    (define-key map (kbd "i w") #'diary-insert-weekly-entry)
    (define-key map (kbd "i y") #'diary-insert-yearly-entry)
    (define-key map (kbd "M-n") #'calendar-forward-month)
    (define-key map (kbd "M-p") #'calendar-backward-month))
  (let ((map global-map))
    (define-key map (kbd "C-c d c") #'calendar)
    (define-key map (kbd "C-c d i") #'diary-insert-entry)
    (define-key map (kbd "C-c d m") #'diary-mail-entries))
  )

(lp-emacs-builtin-package 'appt
  ;; (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)

  (appt-activate 1))

(lp-emacs-builtin-package 'timeclock
  (let ((map global-map))
    (define-key map (kbd "C-c k i") #'timeclock-in)
    (define-key map (kbd "C-c k o") #'timeclock-out)
    (define-key map (kbd "C-c k c") #'timeclock-change))
  (setq timeclock-file "~/dropbox/denotes/timelog")
  (setq timeclock-project-list '(research homework reading)))

;;; modified from prot. while its a great idea, it can be problematic to automatically email from different (work) computers
;; The idea is to get a reminder via email when I launch Emacs in the
;; morning and this file is evaluated.  Obviously this is not a super
;; sophisticated approach, though I do not need one.
;; (let ((time (string-to-number (format-time-string "%H"))))
;;   (when (and (> time 4) (< time 9))
;;     (run-at-time (* 60 5) nil #'diary-mail-entries)))

(lp-emacs-builtin-package 'holidays)


(provide 'lp-calendar)

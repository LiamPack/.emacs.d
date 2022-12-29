;;; timers
(lp-emacs-elpa-package 'tmr
  ;; Works on most unix-based systems I think, unsure on Mac
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list
        (list "Readings"
              "Homework"
              "Work"
              "Break"
              "Pomo"))

  (let ((map global-map))
    (define-key map (kbd "C-c t t") #'tmr)
    (define-key map (kbd "C-c t d") #'tmr-with-description)
    (define-key map (kbd "C-c t T") #'tmr-clone)
    (define-key map (kbd "C-c t c") #'tmr-cancel)
    (define-key map (kbd "C-c t r") #'tmr-remove-finished)
    (define-key map (kbd "C-c t l") #'tmr-tabulated-view)))

;;; time on the modeline
(lp-emacs-builtin-package 'time
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("America/New_York" "New York (USA)")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ))
  (setq display-time-default-load-average 0
        display-time-use-mail-icon t
        display-time-24hr-format t
        display-time-day-and-date t)

  (display-time-mode 1))

(provide 'lp-time)

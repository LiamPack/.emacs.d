(require 'calendar)

;;; LP Journal -- simple journal management
(defvar lp--journal-dir (file-truename "~/org/roam/daily/"))

(defun lp--time-to-string (T)
  ""
  (let* ((time (decode-time T))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (calendar-date-string (list month day year))))

(defun lp--time-to-string-verbose (T)
  ""
  (let* ((time (decode-time T))
         (second (nth 0 time))
         (minute (nth 1 time))
         (hour (nth 2 time))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (format "%4s%.2d%.2d_%.2d%.2d%.2d" year month day hour minute second)))

(defun lp--string-to-time (s)
  ""
  (parse-time-string s))

(defun lp--current-date-string ()
  ""
  (lp--time-to-string (current-time)))

(defun lp--time-string-back-one-day (s)
  ""
  (time-subtract (lp--string-to-time s) (days-to-time 1)))

(defun lp--time-string-forward-one-day (s)
  ""
  (time-add (lp--string-to-time s) (days-to-time 1)))

(defun lp--get-journal-file-date ()
  ""
  (cond
   ((string= "org" (file-name-extension (buffer-file-name)))
    (save-excursion
      ;; Review: Change to perform a regex match and grab first `match-string'. E.g. use
      ;; `rx' on current line and get (`match-string' 1 reg).
      (goto-char (point-min))
      (forward-char 9)
      (buffer-substring-no-properties (point) (save-excursion (move-end-of-line nil) (point)))))
   (t
    (print (format "Unsupported file type %s" (file-name-extension (buffer-file-name)))))
   ))

(defun lp-journal-visit-today ()
  ""
  (interactive)
  (let* ((date-string (lp--current-date-string))
         (filename  (concat lp--journal-dir date-string ".org"))
         (header (if (file-exists-p filename)
                     ""
                   (format "#+title: %s\n" date-string))))
    (find-file filename)
    (insert header)
    (goto-char (point-max))))

(defun lp--get-journal-files-next-date (criterion date)
  ""
  (if-let ((sorted-journal-files (sort (directory-files lp--journal-dir) criterion))
           (result (member (concat (lp--get-journal-file-date) ".org") sorted-journal-files)))
      (find-file (concat lp--journal-dir (cadr result)))
    (print "No date before current file")))

(defun lp-journal-visit-backward-one-day ()
  ""
  (interactive)
  (lp--get-journal-files-next-date #'string> (lp--get-journal-file-date)))

(defun lp-journal-visit-forward-one-day ()
  ""
  (interactive)
  (lp--get-journal-files-next-date #'string< (lp--get-journal-file-date)))


;;; LP Notes -- Simple note management
(defvar lp--notes-dir (file-truename "~/org/roam/"))

(defun lp-notes-dired ()
  ""
  (interactive)
  (dired lp--notes-dir))

(defun lp-notes-find-file ()
  ""
  (interactive)
  (let* ((filename (completing-read "Filename: " (directory-files lp--notes-dir nil ".org") nil t)))
    (find-file (concat (file-name-as-directory lp--notes-dir) filename))))

(defun lp-notes-make-file ()
  ""
  (interactive)
  (let* ((note-name (completing-read "File Title: " '()))
         (note-filename (replace-regexp-in-string (regexp-quote "[ ,.&%$#@!]") "-" (downcase note-name) t t))
         (note-tags (completing-read-multiple "Tags: " '()))
         (unique-time (lp--time-to-string-verbose (current-time)))
         (note-full-filename (concat (file-name-as-directory lp--notes-dir)
                                     (string-join (list unique-time
                                                        (string-join note-tags "+")
                                                        note-filename)
                                                  "--")
                                     ".org")))
    (switch-to-buffer
     (find-file
      note-full-filename))
    (insert
     (format
      "#+title:     %s
#+date:      %s
#+category:  %s
#+orig_name: %s
#+orig_id:   %s
"
      (upcase note-name)
      (lp--time-to-string (current-time))
      note-tags
      note-full-filename
      unique-time))))

(provide 'lp-scratch)

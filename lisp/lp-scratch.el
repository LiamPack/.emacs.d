(require 'calendar)

(defvar journal-dir (file-truename "~/org/journal/"))

(defun lp--visit-today-journal-file ()
  (interactive)
  (let* ((date-string-list (mapcar #'int-to-string (calendar-current-date)))
         (year (nth 2 date-string-list))
         (day (nth 1 date-string-list))
         (month (nth 0 date-string-list))
         (date-string (concat year "-" month "-" day)))
    (find-file
     (concat journal-dir date-string ".org"))
    (insert (format "#+TITLE: %s\n" date-string))))



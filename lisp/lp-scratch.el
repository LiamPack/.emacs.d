(require 'calendar)

(defvar journal-dir (file-truename "~/org/journal/"))

(defun lp--int-to-string (i)
  (if (< i 10)
      (concat "0" (int-to-string i))
    (int-to-string i)))

(defun lp--visit-today-journal-file ()
  (interactive)
  (let* ((date-string-list (mapcar #'lp--int-to-string (calendar-current-date)))
         (year (nth 2 date-string-list))
         (day (nth 1 date-string-list))
         (month (nth 0 date-string-list))
         (date-string (concat year "-" month "-" day))
         (filename  (concat journal-dir date-string ".org"))
         (header (if (file-exists-p filename)
                     ""
                   (format "#+title: %s\n" date-string))))
    (find-file filename)
    (insert header)
    (end-of-buffer)))

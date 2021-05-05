(require 'recentf)
(require 'prot-common)

;;;###autoload
(defun prot-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar prot-recentf--history-files '()
  "Minibuffer history for prot-recentf files.")

(defvar prot-recentf--history-dirs '()
  "Minibuffer history for prot-recentf directories.")

(defun prot-recentf--files ()
  "Return completion table with files in `recentf-list'."
  (prot-common-completion-table
   'file
   (mapcar 'abbreviate-file-name recentf-list)))

(defun prot-recentf--files-prompt (files)
  "Helper of `prot-recentf-recent-files' to read FILES."
  (let ((def (car prot-recentf--history-files)))
    (completing-read
     (format "Recentf [%s]: " def)
     files nil t nil 'prot-recentf--history-files def)))

;;;###autoload
(defun prot-recentf-recent-files (file)
  "Select FILE from `recentf-list' using completion."
  (interactive
   (list (prot-recentf--files-prompt (prot-recentf--files))))
  (find-file file)
  (add-to-history 'prot-recentf--history-files file))

(defun prot-recentf--dirs ()
  "Return completion table with directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (prot-common-completion-table
     'file
     (delete-dups
      (mapcar (lambda (file)
                (if (file-directory-p file)
                    (directory-file-name file)
                  (substring (file-name-directory file) 0 -1)))
              list)))))

(defun prot-recentf--dirs-prompt (dirs)
  "Helper of `prot-recentf-recent-dirs' to read DIRS."
  (let ((def (car prot-recentf--history-dirs)))
    (completing-read
     (format "Recent dir [%s]: " def)
     dirs nil t nil 'prot-recentf--history-dirs def)))

;;;###autoload
(defun prot-recentf-recent-dirs (dir)
  "Select DIR from `recentf-list' using completion."
  (interactive
   (list (prot-recentf--dirs-prompt (prot-recentf--dirs))))
  (find-file dir)
  (add-to-history 'prot-recentf--history-dirs dir))

(provide 'prot-recentf)
;;; prot-recentf.el ends here

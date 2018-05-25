
                                        ; writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;; highlights bad word choices and does stuff
(use-package writegood-mode
  :ensure t
  :defer t
  :hook text-mode
  :diminish writegood-mode
  :bind (("C-c g" . writegood-mode)
         ("\C-c\C-gg" . writegood-grade-level)
         ("\C-c\C-ge" . writegood-reading-ease))
  :config
  (add-to-list 'writegood-weasel-words "actionable"))


;; TODO - decide whether to use this or not
;; More badword highlighting! -


                                        ; toy areas of computer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lp/open-challenges-notes ()
  "Open the org TODO list."
  (interactive)
  (find-file "~/Dropbox/org/dailies.org")
  (flycheck-mode -1))

(global-set-key  (kbd "C-c y") 'lp/open-challenges-notes)
(provide 'use-writing)

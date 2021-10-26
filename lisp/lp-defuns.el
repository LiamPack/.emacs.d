                                        ; useful functions + keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14 ")

(defun lp/kill-current-buffer ()
  "Just kill the buffer man (no prompt when killing buffer)"
  (interactive)
  (kill-buffer (current-buffer)))

(defun lp/generate-scratch-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (emacs-lisp-mode))

;; Allow newly created scratch buffers to be elisp mode
(add-to-list 'auto-mode-alist '("^scratch-.*$" . emacs-lisp-mode))

(defun lp/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on whitespace content. Does
  not indent buffer!"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))
(defun lp/cleanup-buffer ()
  "Perform bunch of operations on the whitespace content of buffer."
  (interactive)
  (lp/cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

;; (defun lp/org-open-point ()
;;   "Open org mode heading in another window, expand it, and narrow it"
;;   (interactive)
;;   (org-beginning-of-line)
;;   (setq goal-point (point))
;;   (call-interactively #'clone-indirect-buffer-other-window)
;;   (while (not (= goal-point (point)))
;;     (goto-char goal-point)
;;     (org-beginning-of-line)
;;     (org-cycle)
;;     (goto-char goal-point)
;;     (org-beginning-of-line))
;;   (call-interactively #'org-next-visible-heading)
;;   (narrow-to-region goal-point (point))
;;   (goto-char goal-point)
;;   (fset 'tab
;;         (lambda (&optional arg) "Keyboard macro." (interactive "p")
;;           (kmacro-exec-ring-item (quote ([tab] 0 "%d")) arg)))
;;   (tab)) 

;; (global-set-key (kbd "C-c o") 'lp/org-open-point)

;; Always killcurrent buffer
(global-set-key (kbd "C-x k") 'lp/kill-current-buffer)

;;(global-set-key (kbd "M-TAB") 'hippie-expand)

(global-set-key (kbd "C-m") 'newline-and-indent)

;; pop to the last command mark! its cool.

(provide 'lp-defuns)

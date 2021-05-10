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

(defun lp/org-open-point ()
  "Open org mode heading in another window, expand it, and narrow it"
  (interactive)
  (org-beginning-of-line)
  (setq goal-point (point))
  (call-interactively #'clone-indirect-buffer-other-window)
  (while (not (= goal-point (point)))
    (goto-char goal-point)
    (org-beginning-of-line)
    (org-cycle)
    (goto-char goal-point)
    (org-beginning-of-line))
  (call-interactively #'org-next-visible-heading)
  (narrow-to-region goal-point (point))
  (goto-char goal-point)
  (fset 'tab
        (lambda (&optional arg) "Keyboard macro." (interactive "p")
          (kmacro-exec-ring-item (quote ([tab] 0 "%d")) arg)))
  (tab)) 

(global-set-key (kbd "C-c o") 'lp/org-open-point)

;; Always killcurrent buffer
(global-set-key (kbd "C-x k") 'lp/kill-current-buffer)

;;(global-set-key (kbd "M-TAB") 'hippie-expand)
(global-set-key (kbd "C-j") #'join-line) ; note that paredit binds this to (paredit-newline)
(global-set-key (kbd "M-g M-g") #'goto-line)
(global-set-key (kbd "C-c n") #'lp/cleanup-buffer)
(global-set-key (kbd "<f12>") #'lp/generate-scratch-buffer)
(global-set-key (kbd "C-c C-k") #'eval-buffer)
(global-set-key (kbd "C-<f7>") #'compile)
(global-set-key (kbd "<f5>")  #'revert-buffer)
(global-set-key (kbd "C-;") #'comment-or-uncomment-region)
(global-set-key (kbd "C-c e") #'eval-and-replace) ; this one is pretty cool.
(global-set-key (kbd "C-x p") #'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; When popping the mark, continue popping until the cursor actually
;; moves Also, if the last command was a copy - skip past all the
;; expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(global-set-key (kbd "C-m") 'newline-and-indent)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config (which-key-mode 1))

;; pop to the last command mark! its cool.

(provide 'lp-defuns)

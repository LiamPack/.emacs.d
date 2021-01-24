                                        ; mc tips and tricks!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
(use-package multiple-cursors
  :disabled
  :straight t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map region-bindings-mode-map
              ("a" . mc/mark-all-like-this)
              ("p" . mc/mark-previous-like-this)
              ("n" . mc/mark-next-like-this)
              ("P" . mc/unmark-previous-like-this)
              ("N" . mc/unmark-next-like-this)
              ("[" . mc/cycle-backward)
              ("]" . mc/cycle-forward)
              ("m" . mc/mark-more-like-this-extended)
              ("h" . mc-hide-unmatched-lines-mode)
              ("\\" . mc/vertical-align-with-space)
              ("#" . mc/insert-numbers) ; use num prefix to set the starting number
              ("^" . mc/edit-beginnings-of-lines)
              ("$" . mc/edit-ends-of-lines))
  :init
  (setq mc/list-file (locate-user-emacs-file "mc-lists"))

  ;; Disable the annoying sluggish matching paren blinks for all cursors
  ;; when you happen to type a ")" or "}" at all cursor locations.
  (defvar modi/mc-blink-matching-paren--store nil
    "Internal variable used to restore the value of `blink-matching-paren'
after `multiple-cursors-mode' is quit.")

  ;; The `multiple-cursors-mode-enabled-hook' and
  ;; `multiple-cursors-mode-disabled-hook' are run in the
  ;; `multiple-cursors-mode' minor mode definition, but they are not declared
  ;; (not `defvar'd). So do that first before using `add-hook'.
  (defvar multiple-cursors-mode-enabled-hook nil
    "Hook that is run after `multiple-cursors-mode' is enabled.")
  (defvar multiple-cursors-mode-disabled-hook nil
    "Hook that is run after `multiple-cursors-mode' is disabled.")

  (defun modi/mc-when-enabled ()
    "Function to be added to `multiple-cursors-mode-enabled-hook'."
    (setq modi/mc-blink-matching-paren--store blink-matching-paren)
    (setq blink-matching-paren nil))

  (defun modi/mc-when-disabled ()
    "Function to be added to `multiple-cursors-mode-disabled-hook'."
    (setq blink-matching-paren modi/mc-blink-matching-paren--store))

  (add-hook 'multiple-cursors-mode-enabled-hook #'modi/mc-when-enabled)
  (add-hook 'multiple-cursors-mode-disabled-hook #'modi/mc-when-disabled))

(use-package expand-region
  :straight t
  :bind ("C-," . er/expand-region))

(use-package zop-to-char                ; Better zapping
  :straight t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree                  ; Branching undo
  :straight t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(provide 'use-mc)

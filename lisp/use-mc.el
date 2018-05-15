                                        ; mc tips and tricks!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     To get out of multiple-cursors-mode, press <return> or C-g. The
;;     latter will first disable multiple regions before disabling
;;     multiple cursors. If you want to insert a newline in
;;     multiple-cursors-mode, use C-j.

;;     (define-key mc/keymap (kbd "<return>") nil) will make <return>
;;     insert a newline; multiple-cursors-mode can still be disabled
;;     with C-g.

;;     Sometimes you end up with cursors outside of your view. You can
;;     scroll the screen to center on each cursor with C-v and M-v or
;;     you can press C-' to hide all lines without a cursor, press C-'
;;     again to unhide.

;;     Try pressing mc/mark-next-like-this with no region selected. It
;;     will just add a cursor on the next line.

;;     Try pressing mc/mark-next-like-this-word or
;;     mc/mark-next-like-this-symbol with no region selected. It will
;;     mark the word or symbol and add a cursor at the next occurance

;;     Try pressing mc/mark-all-like-this-dwim on a tagname in
;;     html-mode.

;;     Notice that the number of cursors active can be seen in the
;;     modeline.

;;     If you get out of multiple-cursors-mode and yank - it will yank
;;     only from the kill-ring of main cursor. To yank from the
;;     kill-rings of every cursor use yank-rectangle, normally found
;;     at C-x r y.

;;     You can use mc/reverse-regions with nothing selected and just
;;     one cursor. It will then flip the sexp at point and the one
;;     below it.

;;     When you use mc/edit-lines, you can give it a positive or
;;     negative prefix to change how it behaves on too short lines.

;;     If you would like to keep the global bindings clean, and get
;;     custom keybindings when the region is active, you can try
;;     region-bindings-mode.

;; BTW, I highly recommend adding mc/mark-next-like-this to a key
;; binding that's right next to the key for er/expand-region.
(require 'use-package)
(use-package multiple-cursors
  :ensure t
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
  :ensure t
  :bind ("C-," . er/expand-region))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree                  ; Branching undo
  :disabled t
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(provide 'use-mc)

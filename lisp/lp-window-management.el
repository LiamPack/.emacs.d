                                        ; window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management - Balance, switching, and splitting
(require 'use-package)

;; Fonts
;; iosevka, consolas, source code pro, Fira Code, dejavu, IBM 3270,
;; Fantasque Sans Mono, Terminus, overpass mono
;; meslo LG / menlo
(setq lp/default-font "Consolas")
(setq lp/default-font-size 12)
(setq lp/current-font-size lp/default-font-size)

;; Define the factor that we should go by when increasing/decreasing
(setq lp/font-change-increment 1.1)

(defun lp/set-font-size ()
  "Set the font to 'lp/default-font' at 'lpcurrent-font-size'."
  (set-frame-font
   (concat lp/default-font "-" (number-to-string lp/current-font-size))))

(defun lp/reset-font-size ()
  "Change font back to default size"
  (interactive)
  (setq lp/current-font-size lp/default-font-size)
  (lp/set-font-size))


(defun lp/increase-font-size ()
  "increase current font size by a factor of 'lp/font-change-increment'."
  (interactive)
  (setq lp/current-font-size
        (ceiling (* lp/current-font-size lp/font-change-increment)))
  (lp/set-font-size))

(defun lp/decrease-font-size ()
  (interactive)
  (setq lp/current-font-size
        (floor (/ lp/current-font-size lp/font-change-increment)))
  (lp/set-font-size))

(define-key global-map (kbd "C-0") 'lp/reset-font-size)
(define-key global-map (kbd "C-=") 'lp/increase-font-size)
(define-key global-map (kbd "C--") 'lp/decrease-font-size)

(lp/reset-font-size)

;; global-hl-line-mode softly highlights bg color of line.
(when window-system
  (global-hl-line-mode))

;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t               ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
		  (abbreviate-file-name (buffer-file-name)) "%b"))
      ;; Size new windows proportionally wrt other windows
      window-combination-resize t)


;; I almost always want to switch to a window when I split. So lets do that.
(defun lp/split-window-below-and-switch ()
  "Split window horizontally, then switch to that new window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun lp/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'lp/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'lp/split-window-right-and-switch)
;; Standard window commands
(bind-key "C-c w =" #'balance-windows)
(bind-key "C-c w k" #'delete-window)
(bind-key "C-c w m" #'delete-other-windows)


;; ace-window stuff
;; You can also start by calling ace-window and then decide to switch the action to delete or swap etc. By default the bindings are:
;;     x - delete window
;;     m - swap windows
;;     M - move window
;;     j - select buffer
;;     n - select the previous window
;;     u - select buffer in the other window
;;     c - split window fairly, either vertically or horizontally
;;     v - split window vertically
;;     b - split window horizontally
;;     o - maximize current window
;;     ? - show these command bindings
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :config
  (setq  aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
	;; Put REPLs and error lists into the bottom side window
	(,(rx bos
              (or "*Help"                         ; Help buffers
		  "*Warnings*"                    ; Emacs warnings
		  "*Compile-Log*"                 ; Emacs byte compiler log
		  "*compilation"                  ; Compilation buffers
		  "*Flycheck errors*"             ; Flycheck error list
		  "*shell"                        ; Shell window
		  "*sbt"                          ; SBT REPL and compilation buffer
		  "*ensime-update*"               ; Server update from Ensime
		  "*SQL"                          ; SQL REPL
		  "*Cargo"                        ; Cargo process buffers
		  (and (1+ nonl) " output*")      ; AUCTeX command output
		  ))
	 (display-buffer-reuse-window
	  display-buffer-in-side-window)
	 (side            . bottom)
	 (reusable-frames . visible)
	 (window-height   . 0.33))
	;; Let `display-buffer' reuse visible frames for all buffers.  This must
	;; be the last entry in `display-buffer-alist', because it overrides any
	;; later entry with more specific actions.
	("." nil (reusable-frames . visible))))

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :straight t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package ibuffer                    ; Better buffer list
  :straight t
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq
   ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 16 :left)
           " "
           filename-and-process)
     (mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :straight t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :straight t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))


;;; experimental TODO
(use-package golden-ratio               ; Automatically resize windows
  :straight t
  :init
  (defun lunaryorn-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . lunaryorn-toggle-golden-ratio))
  :config
  (setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   ;; Exclude a couple of special modes from golden ratio, namely
   ;; Flycheck's error list, calc
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                dired-mode
                                ediff-mode
                                )
   golden-ratio-exclude-buffer-regexp
   `(,(rx bos "*which-key*" eos)
     ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

(provide 'lp-window-management)

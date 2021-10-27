                                        ; window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management - Balance, switching, and splitting
(require 'use-package)

;; Fonts
;; iosevka, consolas, source code pro, Fira Code, dejavu, IBM 3270,
;; Fantasque Sans Mono, Terminus, overpass mono
;; meslo LG / menlo
(set-frame-font "deja vu sans mono 14")

;; global-hl-line-mode softly highlights bg color of line.
;; (when window-system
;;   (global-hl-line-mode))

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

;; ;; Configure `display-buffer' behaviour for some special buffers.
;; (setq display-buffer-alist
;;       `(
;;         ;; Put REPLs and error lists into the bottom side window
;;         (,(rx bos
;;               (or "*Help"                         ; Help buffers
;;                   "*Warnings*"                    ; Emacs warnings
;;                   "*Compile-Log*"                 ; Emacs byte compiler log
;;                   "*compilation"                  ; Compilation buffers
;;                   "*Flycheck errors*"             ; Flycheck error list
;;                   "*shell"                        ; Shell window
;;                   "*sbt"                          ; SBT REPL and compilation buffer
;;                   "*ensime-update*"               ; Server update from Ensime
;;                   "*SQL"                          ; SQL REPL
;;                   "*Cargo"                        ; Cargo process buffers
;;                   (and (1+ nonl) " output*")      ; AUCTeX command output
;;                   ))
;;          (display-buffer-reuse-window
;;           display-buffer-in-side-window)
;;          (side            . bottom)
;;          (reusable-frames . visible)
;;          (window-height   . 0.33))
;;         ;; Let `display-buffer' reuse visible frames for all buffers.  This must
;;         ;; be the last entry in `display-buffer-alist', because it overrides any
;;         ;; later entry with more specific actions.
;;         ("." nil (reusable-frames . visible))))

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

(use-package tab-bar
  :disabled
  :init
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode -1)
  :bind (("<prior>" . tab-next)
         ("<next>" . tab-previous)))


(use-package register)
(use-package desktop
  :config
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save "\\(.*magit.*\\)")
  (setq desktop-modes-not-to-save '(magit-mode magit-status-mode help-mode))
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 20)
  (setq desktop-restore-frames t)
  (setq desktop-save 'ask-if-new)
  (desktop-save-mode 1))

(provide 'lp-window-management)

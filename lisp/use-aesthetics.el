;;; General aesthetic configurations for emacs
;;
;; espresso ; cyberpunk ; moe-light ;
;;* good themes
;;** base16
;;    * zenburn
;;    * unikitty light
;;    * solarized light
;;    * rebecca
;;    * porple
;;    * phd
;;    * ocean
;;    * nord
;;    * monokai
;;    * mocha
;;    * mellow-purple
;;    * material + material palenight
;;    * harmonic-{light,dark}
;;    * cupertino
;;    * cupcake
;;    * sulphurpool-light
;;    * heath-light
;;    * cave-light
;;    * classic-{dark,light}
;; * avk-daylight
;;
;;* actual good themes
;;  * leuven
;;  * porple
;;  * doom-tomorrow-night
;;    * Any of the doom ones really
;;  * Habamax Theme - a little plain
;;  * Also hydanatantantatna-theme
(require 'use-package)
(load-theme 'leuven)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/modern-themes")


;; sick of this blinking
(blink-cursor-mode -1)

;; stop truncating lines
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

;; powerline is terrific

;; also fonts
(set-locale-environment "UTF-8")
;;; API


;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (powerline-buffer-size face0 'l)
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left face0 face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (center (list (powerline-raw global-mode-string face1 'r)
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 face0)
                                          (powerline-raw " ")
                                          (powerline-raw "%6p" face0 'r)
                                          (powerline-hud face2 face1)
                                          ))
                            (rhs (list (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face2 'l))
                                       (powerline-major-mode face2 'l)
                                       (powerline-process face2)
                                       (powerline-raw " :" face2)
                                       (powerline-minor-modes face2 'l)
                                       (powerline-raw " " face2)
                                       (funcall separator-right face2 face1)
                                       ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-right-theme))



(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(use-package moe-theme
  :disabled
  :ensure t
  :config
  (setq moe-light-pure-white-background-in-terminal t)
  (moe-theme-set-color 'purple)
  ;; Resize titles
  (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
  (require 'moe-theme-switcher)
                                        ;(setq moe-theme-highlight-buffer-id t)
  )

;; wrap visual lines! it helps.
(global-visual-line-mode 1)

;; Fancy lambdas
(global-prettify-symbols-mode t)

;; screw the bell
(setq ring-bell-function 'ignore)

;; Scroll conservatively
(setq scroll-conservatively 100)

;; time on modeline is cool
(use-package time                       ; Show current time
  :ensure t
  :bind (("C-c w t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)")))
  (setf display-time-default-load-average nil
        display-time-use-mail-icon t
        display-time-24hr-format t)
  (display-time-mode))


;; Font functionality

;; iosevka, consolas, source code pro, Fira Code, dejavu, IBM 3270,
;; Fantasque Sans Mono
(setq lp/default-font "Fantasque Sans Mono")

(setq lp/default-font-size 16)

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

;; global-hl-line-mode softly highlights bg color of line. Its nice.
(when window-system
  (global-hl-line-mode))

;; Helps with stupid ^L characters - allows a page break to appear!
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

;;; Hide a whole bunch of stuff on the modeline. It's a bit annoying.
;;; Using the =diminish= package for this.
(use-package diminish
  :ensure t
  :config
  (defmacro diminish-minor-mode (filename mode &optional abbrev)
    `(eval-after-load (symbol-name ,filename)
       '(diminish ,mode ,abbrev)))

  (defmacro diminish-major-mode (mode-hook abbrev)
    `(add-hook ,mode-hook
               (lambda () (setq mode-name ,abbrev))))

  (diminish-minor-mode 'abbrev 'abbrev-mode)
  (diminish-minor-mode 'simple 'auto-fill-function)
  (diminish-minor-mode 'company 'company-mode)
  (diminish-minor-mode 'eldoc 'eldoc-mode)
  (diminish-minor-mode 'flycheck 'flycheck-mode)
  (diminish-minor-mode 'flyspell 'flyspell-mode)
  (diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
  (diminish-minor-mode 'projectile 'projectile-mode)
  (diminish-minor-mode 'ruby-end 'ruby-end-mode)
  (diminish-minor-mode 'subword 'subword-mode)
  (diminish-minor-mode 'undo-tree 'undo-tree-mode)
  (diminish-minor-mode 'yard-mode 'yard-mode)
  (diminish-minor-mode 'yasnippet 'yas-minor-mode)
  (diminish-minor-mode 'wrap-region 'wrap-region-mode)
  (diminish-minor-mode 'simple 'visual-line-mode)
  (diminish-minor-mode 'paredit 'paredit-mode " π")
  (diminish-major-mode 'emacs-lisp-mode-hook "el")
  (diminish-major-mode 'haskell-mode-hook "λ=")
  (diminish-major-mode 'lisp-interaction-mode-hook "λ")
  (diminish-major-mode 'python-mode-hook "Py"))



(provide 'use-aesthetics)

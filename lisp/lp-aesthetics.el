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
;;  * leuven / parchment (https://github.com/ajgrf/parchment)
;;  * porple
;;  * doom-tomorrow-night
;;    * Any of the doom ones really
;;  * Habamax Theme - a little plain
;;  * Also hydanatantantatna-theme
;;  * gruvbox
;;  * tsdh-light
;;  * tron theme https://github.com/ianpan870102/Emacs-Tron-Legacy-Theme
;;  * Naysayer-theme https://github.com/nickav/naysayer-theme.el
;;  * That one black theme i'm using right now (6/15/19)
(require 'use-package)

(use-package sexy-monochrome-theme
  :disabled
  :straight t
  :config
  (load-theme 'sexy-monochrome))
(use-package twilight-bright-theme
  :disabled
  :straight t
  :config
  (load-theme 'twilight-bright))
(use-package twilight-anti-bright-theme
  :disabled
  :straight t)
(use-package lenlen-theme
  :disabled
  :straight t)

;; Taken directly from Prot
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-no-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-success-deuteranopia nil

        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense'
        modus-themes-lang-checkers '(straight-underline background intense)

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented'.  The variable's doc string shows all possible
        ;; combinations.
        modus-themes-mode-line '(moody borderless accented)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax '(alt-syntax yellow-comments)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(accented intense)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline background)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}

        modus-themes-mail-citations nil ; {nil,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(no-extend accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated,
        ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
        modus-themes-diffs 'fg-only-deuteranopia

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch scale-title))
          (header-date . (grayscale workaholic bold-today))
          (scheduled . uniform)
          (habit . traffic-light-deuteranopia))

        modus-themes-headings ; this is an alist: read the manual or its doc string
        '((1 . (overline background))
          (2 . (rainbow overline))
          (t . (no-bold)))

        modus-themes-variable-pitch-ui nil
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-title 1.33)
  :config
  (load-theme 'modus-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; (set-face-attribute 'mode-line nil :background "NavajoWhite")
;; (set-face-attribute 'mode-line-inactive nil :background "#FAFAFA")


;; (use-package avk-emacs-themes
;;   :straight t
;;   :config
;;   (load-theme 'avk-darkblue-yellow))

;; time on modeline is cool
(use-package time                       ; Show current time
  :straight t
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

;; Helps with stupid ^L characters - allows a page break to appear
(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

(provide 'lp-aesthetics)


;; ;; Add all your customizations prior to loading the themes
;; ;;
;; ;; NOTE: these are not my preferences!  I am always testing various
;; ;; configurations.  Though I still like what I have here.
;; (setq modus-themes-slanted-constructs t
;;       modus-themes-bold-constructs t
;;       modus-themes-fringes 'subtle ; {nil,'subtle,'intense}
;;       ;; Options for `modus-themes-lang-checkers': nil,
;;       ;; 'straight-underline, 'subtle-foreground,
;;       ;; 'subtle-foreground-straight-underline, 'intense-foreground,
;;       ;; 'intense-foreground-straight-underline, 'colored-background
;;       modus-themes-lang-checkers nil
;;       modus-themes-mode-line nil ; {nil,'3d,'moody}
;;       ;; Options for `modus-themes-syntax': nil, 'faint,
;;       ;; 'yellow-comments, 'green-strings,
;;       ;; 'yellow-comments-green-strings, 'alt-syntax,
;;       ;; 'alt-syntax-yellow-comments
;;       modus-themes-syntax 'alt-syntax-yellow-comments
;;       modus-themes-intense-hl-line nil
;;       modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
;;       ;; Options for `modus-themes-links': nil, 'faint,
;;       ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
;;       ;; 'underline-only
;;       modus-themes-links 'underline-only
;;       modus-themes-no-mixed-fonts nil
;;       modus-themes-prompts 'subtle ; {nil,'subtle,'intense}
;;       modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
;;       modus-themes-region 'bg-only-no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
;;       modus-themes-diffs 'bg-only ; {nil,'desaturated,'fg-only,'bg-only}
;;       modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
;;       modus-themes-org-habit nil ; {nil,'simplified,'traffic-light}
;;       modus-themes-headings ; Read the manual for this one
;;       '((t . no-color-no-bold))
;;       modus-themes-variable-pitch-ui nil
;;       modus-themes-variable-pitch-headings nil
;;       modus-themes-scale-headings t
;;       modus-themes-scale-1 1.1
;;       modus-themes-scale-2 1.15
;;       modus-themes-scale-3 1.21
;;       modus-themes-scale-4 1.27
;;       modus-themes-scale-5 1.33)
;; ;; Load the theme files before enabling a theme (I do this via the
;; ;; `after-init-hook', though you could also add `(enable-theme
;; ;; 'modus-operandi)' after the `:config' keyword.
;; (modus-themes-load-themes)
;; :config
;; ;; Don't forget to read the manual.  This is explained there.  And
;; ;; please don't read too much into those: I am testing various
;; ;; combinations and scenaria.
;; (defun prot/modus-themes-custom-faces ()
;;   "Tweak faces after `modus-themes-after-load-theme-hook'."
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(dired-flagged ((,class :strike-through t)))
;;      `(gnus-summary-cancelled ((,class :strike-through t)))
;;      `(line-number ((,class :background unspecified :foreground ,fg-unfocused)))
;;      `(line-number-current-line
;;        ((,class :background ,bg-special-cold :foreground ,fg-special-cold))))))

;; ;; Have I told you about the manual?  This is just meant to showcase
;; ;; the option of overriding individual colours from each theme's
;; ;; palette.
;; ;; (define-minor-mode prot/modus-themes-tinted
;; ;;   "Tweak key Modus themes colors."
;; ;;   :init-value nil
;; ;;   :global t
;; ;;   (if prot/modus-themes-tinted
;; ;;       (setq modus-themes-operandi-color-overrides
;; ;;             '((bg-main . "#fefcf4")
;; ;;            (bg-dim . "#faf6ef")
;; ;;            (bg-alt . "#f7efe5")
;; ;;            (bg-hl-line . "#f4f0e3")
;; ;;            (bg-active . "#e8dfd1")
;; ;;            (bg-inactive . "#f6ece5")
;; ;;            (bg-region . "#c6bab1")
;; ;;            (bg-header . "#ede3e0")
;; ;;            (bg-tab-bar . "#dcd3d3")
;; ;;            (bg-tab-active . "#fdf6eb")
;; ;;            (bg-tab-inactive . "#c8bab8")
;; ;;            (fg-unfocused . "#55556f"))
;; ;;             modus-themes-vivendi-color-overrides
;; ;;             '((bg-main . "#100b17")
;; ;;            (bg-dim . "#161129")
;; ;;            (bg-alt . "#181732")
;; ;;            (bg-hl-line . "#191628")
;; ;;            (bg-active . "#282e46")
;; ;;            (bg-inactive . "#1a1e39")
;; ;;            (bg-region . "#393a53")
;; ;;            (bg-header . "#202037")
;; ;;            (bg-tab-bar . "#262b41")
;; ;;            (bg-tab-active . "#120f18")
;; ;;            (bg-tab-inactive . "#3a3a5a")
;; ;;            (fg-unfocused . "#9a9aab")))
;; ;;     (setq modus-themes-operandi-color-overrides nil
;; ;;           modus-themes-vivendi-color-overrides nil)))

;; ;; Toggle the minor mode and switch between the themes to see the
;; ;; effect.
;; ;;  (prot/modus-themes-tinted -1)

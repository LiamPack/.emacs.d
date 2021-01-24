
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package magic-latex-buffer
  :straight t
  :config
  (add-hook 'latex-mode-hook 'magic-latex-buffer)
  ;; (setq magic-latex-enable-block-highlight nil
  ;;       magic-latex-enable-suscript        t
  ;;       magic-latex-enable-pretty-symbols  t)
  )

(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-PDF-mode t)
  ;; The damn text is too tiny on the preview.
  (set-default 'preview-scale-function 3.0)

  ;; revert pdf-view after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  ;; (setq TeX-view-program-list
  ;;       '(("Evince" "evince --page-index=%(outpage) %o")))
  ;; (setq TeX-view-program-selection '((output-pdf "Evince")))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; TODO Alt config!
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-save-query nil)
  (setq-default TeX-command-extra-options "--shell-escape")
  (setq TeX-error-overview-open-after-TeX-run t)
  (setq TeX-electric-math '("$" . "$"))
  (setq TeX-electric-sub-and-superscript t)
  ;; fix for company completion
  ;; (define-key TeX-mode-map (kbd "TAB") 'tab-indent-or-complete)
  ;; (define-key TeX-mode-map [tab] 'tab-indent-or-complete)

  ;; Fix auto-fill in math mode
  (setq-default LaTeX-fill-break-at-separators (quote (\\\( \\\[ \\\])))

  ;; spelling corrections
  (require 'ispell)
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)

  (setq ispell-dictionary-base-alist
        '(("en_US"
           "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
           ("-d" "en_US" "-i" "iso-8859-1") nil iso-8859-1)
          ("en_GB"
           "[a-zA-Z]" "[^a-zA-Z]" "[']" nil
           ("-d" "en_GB" "-i" "iso-8859-1") nil iso-8859-1)
          ("de_DE"
           "[a-zäöüßA-ZÄÖÜ]" "[^a-zäöüßA-ZÄÖÜ]" "[']" nil
           ("-d" "de_DE" "-i" "iso-8859-1") nil iso-8859-1)))
  (eval-after-load "ispell"
    (progn
      (setq ispell-dictionary "en_US")
      (setq ispell-silently-savep t))) ; save personal dict without confirmation
  )

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  ;; Plug into AUCTeX
  (setq reftex-plug-into-AUCTeX t
        ;; Automatically derive labels, and prompt for confirmation
        reftex-insert-label-flags '(t t)
        reftex-label-alist
        '(
          ;; Additional label definitions for RefTeX.
          ("definition" ?d "def:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("definition" "def.") -3)
          ("theorem" ?h "thm:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("theorem" "th.") -3)
          ("example" ?x "ex:" "~\\ref{%s}"
           lunaryorn-reftex-find-ams-environment-caption
           ("example" "ex") -3)
          ;; Algorithms package
          ("algorithm" ?a "alg:" "~\\ref{%s}"
           "\\\\caption[[{]" ("algorithm" "alg") -3)))

  ;; Provide basic RefTeX support for biblatex
  (unless (assq 'biblatex reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin
                 '(biblatex "The biblatex package"
                            ((?\C-m . "\\cite[]{%l}")
                             (?t . "\\textcite{%l}")
                             (?a . "\\autocite[]{%l}")
                             (?p . "\\parencite{%l}")
                             (?f . "\\footcite[][]{%l}")
                             (?F . "\\fullcite[]{%l}")
                             (?x . "[]{%l}")
                             (?X . "{%l}"))))
    (setq reftex-cite-format 'biblatex))
  :diminish reftex-mode)

(provide 'use-tex)

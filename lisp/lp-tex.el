;; auctex requires special configuration..
;;
;; system needs:
;; - tex installation
;; - texinfo
;; - ghostscript
;; not yet convinced auctex is a necessary component of my workflow. it provides
;; utilities that could easily be provided by snippets and cdlatex. Time will tell
;;
;; set dont-use-auctex-scope to NIL if you want to use auctex
(if-let ((dont-use-auctex-scope nil))
    nil
  (when (not (package-installed-p 'auctex))
    (package-install 'auctex))
  (setq reftex-plug-into-AUCTeX t)
  ;; revert pdf-view after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; for syncing output compilation to buffer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex)

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  ;; most crucial: turn-on-reftex
  (setq lp--latex-hooks-fns '(auto-fill-mode TeX-source-correlate-mode flyspell-mode flyspell-buffer turn-on-reftex prettify-symbols-mode cdlatex-mode))
  (dolist (fn lp--latex-hooks-fns)
    (add-hook 'LaTeX-mode-hook fn)))
;; https://karthinks.com/software/latex-input-for-impatient-scholars/
(lp-emacs-builtin-package 'tex-mode)

(lp-emacs-builtin-package 'reftex)

(lp-emacs-elpa-package 'cdlatex
  (add-hook 'latex-mode-hook #'cdlatex-mode))

(lp-emacs-builtin-package 'bibtex)
(lp-emacs-elpa-package 'bibtex-completion
  (setq bibtex-completion-bibliography '(
                                         "~/org/bib/index.bib"
                                         "~/org/bib/archive.bib"
                                         )
        bibtex-completion-library-path '("~/org/bib/pdfs/")
        bibtex-completion-notes-path "~/org/bib/notes/"))

(lp-emacs-git-package
    'consult-bibtex "https://github.com/mohkale/consult-bibtex.git"
    (with-eval-after-load 'embark
      (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))
    (setq consult-bibtex-default-action #'consult-bibtex-edit-notes))

(provide 'lp-tex)

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

;; posts:
;; https://karthinks.com/software/latex-input-for-impatient-scholars/
;; https://michaelneuper.com/posts/efficient-latex-editing-with-emacs/
(lp-emacs-builtin-package 'tex-mode
  (let ((map tex-mode-map))
    (define-key map (kbd "C-c C-o") nil)
    (define-key map (kbd "C-c C-j") nil)))

(lp-emacs-builtin-package 'bibtex)

;; Must be loaded before auctex.
(setq cdlatex-math-modify-prefix ?`)
(setq cdlatex-math-symbol-prefix ?')
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

  (let ((map TeX-mode-map))
    (define-key map (kbd "C-M-i") nil))

  ;; most crucial: turn-on-reftex
  (setq lp--latex-hooks-fns '(auto-fill-mode TeX-fold-mode TeX-source-correlate-mode
					     flyspell-mode flyspell-buffer prettify-symbols-mode
					     turn-on-reftex turn-on-cdlatex abbrev-mode))
  (dolist (fn lp--latex-hooks-fns)
    (add-hook 'LaTeX-mode-hook fn))
  
  ;; TeX-fold mode all on C-c C-o
  ;; preview-mode all on C-c C-p
  (set-default 'preview-scale-function 1.3))

(lp-emacs-builtin-package 'reftex)
(lp-emacs-elpa-package 'cdlatex)
;; (lp-emacs-elpa-package 'xenops
;;   (add-hook 'LaTeX-mode-hook #'xenops-mode)
;;   (setq xenops-font-height 180)
 
;;   )

;;; TODO: what even is this.
;;; TODO: synchronize with CITAR or remove bibtex-completion.
;; (lp-emacs-elpa-package 'bibtex-completion
;;   (setq bibtex-completion-bibliography '("~/dropbox/grad/research/lock-in-writeups/qft2.bib"
;; 					 "~/dropbox/grad/research/lock-in-writeups/pierre/pierre.bib"
;;                                          )
;;         bibtex-completion-library-path '("~/dropbox/grad/research/lock-in-papers"
;; "~/dropbox/grad/research/lock-in-books"
;; 					 )
;;         bibtex-completion-notes-path "~/dropbox/denotes/"))

(lp-emacs-git-package
    'consult-bibtex "https://github.com/mohkale/consult-bibtex.git"
    (with-eval-after-load 'embark
      (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))
    ;; (setq consult-bibtex-default-action #'consult-bibtex-edit-notes)
    )

(provide 'lp-tex)

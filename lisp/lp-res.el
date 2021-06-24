                                        ; research with org-mode!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)
;; org-ref
(use-package bibtex-utils
  :straight t)

(use-package biblio
  :straight t)

(use-package interleave
  :straight t)

;;(require 'pubmed)
;;(require 'arxiv)
;;(require 'sci-id)

(autoload 'helm-bibtex "helm-bibtex" "" t)

(use-package org-ref
  :straight t
  :config
  (require 'doi-utils)
  (setq org-ref-notes-directory "~/Dropbox/res"
        org-ref-bibliography-notes "~/Dropbox/res/notes.org"
        org-ref-default-bibliography '("~/Dropbox/res/index.bib")
        org-ref-pdf-directory "~/Dropbox/res/lib/"))

(use-package helm-bibtex
  :straight t
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/res/index.bib" ;; where your references are stored
        helm-bibtex-library-path "~/Dropbox/res/lib/"
        bibtex-completion-library-path '("~/Dropbox/res/lib/") ;; where your pdfs etc are stored
        helm-bibtex-notes-path "~/Dropbox/res/notes.org" ;; where your notes are stored
        bibtex-completion-bibliography "~/Dropbox/res/index.bib" ;; completion
        bibtex-completion-notes-path "~/Dropbox/res/notes.org"))


(use-package org-noter
  :straight t)

(provide 'lp-res)

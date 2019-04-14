                                        ; research with org-mode!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)


;; org-ref
(use-package bibtex-utils
  :ensure t)

(use-package biblio
  :ensure t)

(use-package interleave
  :ensure t)
;;(require 'pubmed)
;;(require 'arxiv)
;;(require 'sci-id)

(autoload 'helm-bibtex "helm-bibtex" "" t)

(use-package org-ref

  :ensure t
  :config
  (require 'doi-utils)
  (setq org-ref-notes-directory "~/Dropbox/res"
        org-ref-bibliography-notes "~/Dropbox/res/notes.org"
        org-ref-default-bibliography '("~/Dropbox/res/index.bib")
        org-ref-pdf-directory "~/Dropbox/res/lib/"))

(use-package helm-bibtex

  :ensure t
  :config
  (setq helm-bibtex-bibliography "~/Dropbox/res/index.bib" ;; where your references are stored
        helm-bibtex-library-path "~/Dropbox/res/lib/"
        bibtex-completion-library-path '("~/Dropbox/res/lib/") ;; where your pdfs etc are stored
        helm-bibtex-notes-path "~/Dropbox/res/notes.org" ;; where your notes are stored
        bibtex-completion-bibliography "~/Dropbox/res/index.bib" ;; completion
        bibtex-completion-notes-path "~/Dropbox/res/notes.org"))

(defun lp/open-paper-notes ()
  "Open the org TODO list."
  (interactive)
  (find-file "~/Dropbox/res/notes.org")
  (flycheck-mode -1))
(global-set-key  (kbd "C-c r") 'lp/open-paper-notes)


(use-package org-noter
  :ensure t)

(provide 'use-res)

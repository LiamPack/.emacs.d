(lp-emacs-builtin-package 'abbrev
  (setq abbrev-suggest t)
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil))

(lp-emacs-builtin-package 'dabbrev
  (define-key global-map (kbd "M-/") 'dabbrev-completion)
  (define-key global-map (kbd "C-M-/") 'dabbrev-expand)
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_") ;; same as nil technically
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t))

(lp-emacs-elpa-package 'cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; orderless minibuffer completion
(lp-emacs-elpa-package 'orderless
  (setq completion-styles '(basic orderless))
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles . (basic partial-completion initials substring)))
                                        (project-file (styles . (basic substring partial-completion orderless)))
                                        (imenu (styles . (basic substring orderless)))
                                        (kill-ring (styles . (basic substring orderless)))
                                        (consult-location (styles . (basic substring orderless)))))
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-initialism
                                    orderless-regexp))

  (defun literal-if-comma (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun flex-if-tilde (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun initialism-if-eql (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(literal-if-comma without-if-bang flex-if-tilde initialism-if-eql))

  (define-key minibuffer-local-completion-map (kbd "SPC") nil)
  (define-key minibuffer-local-completion-map (kbd "?") nil)
  ;; SPC should never complete: use it for `orderless' groups.
  )

;;; cross-referencing 
(lp-emacs-builtin-package 'xref
  ;; All these have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'ripgrep)
  )


;;; simple templates for expansion
(lp-emacs-elpa-package 'tempel
  (let ((map global-map))
    (define-key map (kbd "M-+") #'tempel-complete)
    (define-key map (kbd "M-*") #'tempel-insert))
  (let ((map tempel-map))
    (define-key map (kbd "RET") #'tempel-done)
    (define-key map (kbd "C-p") #'tempel-previous)
    (define-key map (kbd "C-n") #'tempel-next))

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )


(lp-emacs-builtin-package 'recentf                    ; Save recently visited files
  ;; :diminish recentf-mode
  (recentf-mode)
  (setq
   recentf-max-saved-items 200
   recentf-max-menu-items 15
   ;; Cleanup recent files only when Emacs is idle, but not when the mode
   ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
   ;; idles often enough to have the recent files list clean up regularly
   recentf-auto-cleanup 300
   recentf-exclude (list "/\\.git/.*\\'"     ; Git contents
                         "/elpa/.*\\'"       ; Package files
                         "/itsalltext/"      ; It's all text temp files
                         )))

(provide 'lp-completion)

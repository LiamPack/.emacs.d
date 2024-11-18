;;;; This file is has perhaps the most ambiguous name. Packages
;;;; specified here are derived from the `minibuffer' group, which you
;;;; can reach via "M-x customize-group RET minibuffer".
;;; minibuffer defaults
(lp-emacs-builtin-package 'minibuffer
  (setq completion-show-inline-help t)
  (setq completions-detailed t)
  (setq completion-ignore-case t)
  (setq completion-auto-wrap t)
  (setq completion-auto-select t)
  (setq completion-auto-help 'visible) ;; TODO tweak
  (setq completions-format 'horizontal)
  (setq completions-max-height 20)
  ;; (setq completions-header-format nil)
  (setq completion-cycle-threshold nil)
  (setq read-minibuffer-restore-windows t)

  ;; emacs28 completion stuff
  (setq completions-group t)
  (setq completions-group-sort nil)

  (setq enable-recursive-minibuffers nil)
  (require 'minibuf-eldef)
  (setq minibuffer-eldef-shorten-default t) ;; default completion in [bracks]

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq resize-mini-windows t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1) ;; update default completion if change

  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq crm-separator "[ 	]*;[ 	]*")

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq suggest-key-bindings t)

  ;; (when (and (>= emacs-major-version 29))
  ;;   (define-key minibuffer-mode-map (kbd "C-n") 'minibuffer-next-completion)
  ;;   (define-key minibuffer-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  ;;   (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  ;;   (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  ;;   (define-key completion-list-mode-map (kbd "C-n") 'next-completion)
  ;;   (define-key completion-list-mode-map (kbd "C-p") 'previous-completion))
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

;;; Enhancing built-in commands with better minibuffer completion
;;; capabilities
(lp-emacs-elpa-package 'consult
  (setq consult-goto-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "e") #'consult-compile-error)
          (define-key map (kbd "l") #'consult-flymake)
          (define-key map (kbd "f") #'consult-flymake)
          (define-key map (kbd "o") #'consult-outline)
          (define-key map (kbd "m") #'consult-mark)
          (define-key map (kbd "k") #'consult-global-mark)
          (define-key map (kbd "i") #'consult-imenu)
          (define-key map (kbd "I") #'consult-imenu-multi)
	  ;; `consult-bibtex' is not yet defined, but the symbol will
	  ;; be when emacs is done loading
          (define-key map (kbd "b") #'consult-bibtex)
          map))

  (let ((map global-map))
    ;; Custom M-# bindings for fast register access
    (define-key map (kbd "C-x r l") #'consult-register-load)
    (define-key map (kbd "C-x r s") #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
    (define-key map (kbd "C-x r r") #'consult-register)
    (define-key map (kbd "C-x r b") #'consult-bookmark)
    (define-key map (kbd "C-x r B") #'list-bookmarks))

  (setq consult-mode-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "h") #'consult-history)
          (define-key map (kbd "m") #'consult-mode-command)
          (define-key map (kbd "k") #'consult-kmacro)
          map))

  (setq consult-search-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "f") #'consult-find)
          (define-key map (kbd "F") #'consult-locate)
          (define-key map (kbd "g") #'consult-grep)
          (define-key map (kbd "G") #'consult-git-grep)
          (define-key map (kbd "r") #'consult-ripgrep)
          (define-key map (kbd "l") #'consult-line)
          (define-key map (kbd "L") #'consult-line-multi)
          (define-key map (kbd "m") #'consult-multi-occur)
          (define-key map (kbd "k") #'consult-keep-lines)
          (define-key map (kbd "u") #'consult-focus-lines)
          (define-key map (kbd "j") #'consult-recent-file)
          (define-key map (kbd "s") #'consult-isearch-history)
          (define-key map (kbd "o") #'occur)
          (define-key map (kbd "C-o") 'occur)
          map))

  (define-key global-map (kbd "M-s") consult-search-map)
  (define-key global-map (kbd "M-j") consult-goto-map)
  (define-key global-map (kbd "M-M") consult-mode-mode-map)
  (define-key global-map (kbd "C-c y l") 'consult-flymake)
  (define-key global-map (kbd "M-g M-g") 'consult-goto-line)             ;; orig. goto-line

  (define-key global-map (kbd "C-x b") 'consult-buffer)
  (define-key global-map (kbd "C-M-y") 'consult-yank-pop)
  (define-key global-map (kbd "C-:") 'consult-complex-command)

  (setq consult-preview-key "C-o")
  ;; (setq consult-preview-key (kbd "C-o") ;; disable live preview
  ;; (setq consult-project-root-function #'project-roots)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key "<")
  (setf (alist-get 'slime-repl-mode consult-mode-histories)
        'slime-repl-input-history)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  (define-key completion-list-mode-map (kbd "C-o") #'consult-preview-at-point)
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode) ;;!?

  (setq completion-in-region-function #'consult-completion-in-region)
  (add-hook 'minibuffer-setup-hook
            #'(lambda () (interactive)
                (setq-local completion-in-region-function #'completion--in-region)))
  )

(lp-emacs-elpa-package 'consult-dir
  (define-key global-map (kbd "C-x C-d") 'consult-dir)
  (define-key minibuffer-local-completion-map (kbd "C-x C-d") 'consult-dir)
  (define-key minibuffer-local-completion-map (kbd "C-x C-j") 'consult-dir-jump-file))


;;; cross-referencing 
(lp-emacs-builtin-package 'xref
  ;; All these have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'ripgrep)
  )

(lp-emacs-elpa-package 'marginalia
  (setq marginalia-max-relative-age 0) ; time is absolute here!
  (marginalia-mode 1))

;;; for when things are really tough, some icomplete
(when (and (<= emacs-major-version 27))
  (lp-emacs-builtin-package 'icomplete
    (icomplete-vertical-mode +1)
    (setq icomplete-compute-delay 2)
    (setq icomplete-max-delay-chars 2)
    (setq icomplete-matches-format "[%s/%s] ")
    (setq icomplete-in-buffer t)
    (setq icomplete-with-completion-tables t)
    (setq icomplete-separator " . ")
    (let ((map icomplete-minibuffer-map))
      (define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
      (define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
      (define-key icomplete-minibuffer-map (kbd "TAB")  #'icomplete-force-complete)
      (define-key map (kbd "C-j") #'icomplete-ret) ;; reverse C-j and <RET> behavior
      (define-key map (kbd "<RET>") #'icomplete-force-complete-and-exit))
    ))


;;; Minibuffer Commander. 
(lp-emacs-elpa-package 'mct
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setq mct-remove-shadowed-file-names t) ; works when `file-name-shadow-mode' is enabled
  (setq mct-hide-completion-mode-line t)
  (setq mct-minimum-input 3)
  (setq mct-live-completion t)
  (setq mct-live-update-delay 0.6)

  ;; This is for commands or completion categories that should always pop
  ;; up the completions' buffer.  It circumvents the default method of
  ;; waiting for some user input (see `mct-minimum-input') before
  ;; displaying and updating the completions' buffer.
  (setq mct-completion-passlist
	'(;; Some commands
          Info-goto-node
          Info-index
          Info-menu
          vc-retrieve-tag
          ;; Some completion categories
          imenu
          file
          buffer
          kill-ring
          consult-location))

  ;; The blocklist follows the same principle as the passlist, except it
  ;; disables live completions altogether.
  (setq mct-completion-blocklist nil)

  (mct-mode +1)

  ;; Define the small wrapper functions
  (defun my-mct-next-line-or-completion (n)
    "Select next completion or move to next line N times.
Select the next completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
	(minibuffer-next-completion n)
      (next-line n)))

  (defun my-mct-previous-line-or-completion (n)
    "Select previous completion or move to previous line N times.
Select the previous completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
	(minibuffer-previous-completion n)
      (previous-line n)))

  (defun my-mct-return-or-choose-completion (n)
    "Choose current completion or create N newlines.
Choose the current completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
	(minibuffer-choose-completion)
      (newline n :interactive)))

  ;; Get the key bindings
  (let ((map completion-in-region-mode-map))
    (define-key map (kbd "C-n") #'my-mct-next-line-or-completion)
    (define-key map (kbd "C-p") #'my-mct-previous-line-or-completion)
    (define-key map (kbd "RET") #'my-mct-return-or-choose-completion))

  ;; Tweak the appearance
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help t)

  ;; Optionally, tweak the appearance further
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-highlight-face 'completions-highlight)

  )


(provide 'lp-minibuffer)

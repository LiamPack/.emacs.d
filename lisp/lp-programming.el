(lp-emacs-builtin-package 'prog-mode
  ;; Mark TODOs , FIXME, BUG as red in src code
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face prepend))))))


;;; `electric' behavior
(lp-emacs-builtin-package 'electric
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))



(lp-emacs-builtin-package 'compile
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'next-error)
  (setq compilation-skip-threshold 2)
  (setq compilation-scroll-output 'first-error)
  (setq compilation-always-kill t)
  (setq compilation-auto-jump-to-first-error t)
  )

(lp-emacs-builtin-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout 1.0)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check nil)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))

  (add-hook 'prog-mode-hook 'flymake-mode)

  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c y s") #'flymake-start)
    (define-key map (kbd "C-c y !") #'flymake-show-buffer-diagnostics) ; Emacs28
    (define-key map (kbd "C-c y n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c y p") #'flymake-goto-prev-error)))


(lp-emacs-elpa-package 'flymake-diagnostic-at-point
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))

  ;;; Flymake + Shellcheck
(lp-emacs-elpa-package 'flymake-shellcheck
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

  ;;; Flymake + Proselint
;; (lp-emacs-elpa-package 'flymake-proselint
;;   (add-hook 'text-mode-hook #'flymake-mode)
;;   (add-hook 'markdown-mode-hook #'flymake-proselint-setup)
;;   (add-hook 'org-mode-hook #'flymake-proselint-setup)
;;   (add-hook 'text-mode-hook #'flymake-proselint-setup))

(lp-emacs-elpa-package 'flymake-python-pyflakes
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;;; pyton :)
(lp-emacs-elpa-package 'python
  (setq python-indent-offset 4
        python-pdbtrack-activate t)
  
  (setq python-shell-interpreter "python"
        python-shell-prompt-detect-failure-warning nil)
  
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--simple-prompt")))

(lp-emacs-elpa-package 'pyvenv)

(when (executable-find "conda")
  (lp-emacs-elpa-package 'conda
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)

    ;; TODO: these need to be based on environment variables from conda
    ;; ~/.conda configuration
    (setq conda-anaconda-home (expand-file-name "~/miniconda3/")
					conda-env-home-directory (expand-file-name "~/miniconda3/")
					conda-env-subdirectory "envs")
    ))

(when (executable-find "jupyter") (lp-emacs-elpa-package 'ein))
(lp-emacs-elpa-package 'code-cells) ; for generic code-block editing

;;; julia
(lp-emacs-elpa-package 'julia-mode)

(lp-emacs-elpa-package 'vterm)

(lp-emacs-elpa-package 'julia-snail
  (add-hook 'julia-mode-hook 'julia-snail-mode)
  (setq julia-snail-multimedia-enable t)
  (setq julia-snail-multimedia-buffer-autoswitch t)
  (setq julia-snail-multimedia-buffer-style :multi)
  (setq julia-snail-extensions '(repl-history formatter))
  (setq julia-snail-repl-display-eval-results t))

;;; java, unfortunately
(add-hook 'java-mode-hook
          #'(lambda ()
              (interactive)
              (setq-local tab-width 2)))

;;; c{,++}
(lp-emacs-builtin-package 'cc-mode
  :config
  (setq gdb-many-windows 't)
  (setq tab-width 2)
  (setq c-basic-offset 4)

  (define-key c-mode-map (kbd "C-j") 'c-indent-new-comment-line)
  (define-key c++-mode-map (kbd "C-j") 'c-indent-new-comment-line)
  (add-hook 'c++-mode-hook
            #'(lambda ()
                (setq compile-command "cmake .. -DCMAKE_EXRORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug; cmake --build . -j8")))
  )

(lp-emacs-builtin-package 'gud
  (setq gud-nav-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "g b") 'gud-break)
          (define-key map (kbd "g <") 'gud-up)
          (define-key map (kbd "g >") 'gud-down)
          (define-key map (kbd "g n") 'gud-next)
          (define-key map (kbd "g s") 'gud-step)
          (define-key map (kbd "g c") 'gud-cont)
          (define-key map (kbd "g p") 'gud-print)
          (define-key map (kbd "g d") 'gud-remove)
          (define-key map (kbd "g l") 'gud-refresh)
          (define-key map (kbd "g e") 'gud-statement)
          map))
  (define-key c-mode-map (kbd "C-c g") gud-nav-map)
  (define-key c++-mode-map (kbd "C-c g") gud-nav-map))

;;; cmake
(lp-emacs-elpa-package 'cmake-mode)

(lp-emacs-elpa-package 'eldoc-cmake
  (add-hook 'cmake-mode-hook 'eldoc-cmake-enable))

;;; ocaml
(lp-emacs-elpa-package 'tuareg
  (setq tuareg-opam-insinuate t))

(lp-emacs-elpa-package 'utop
  (setq utop-command "opam config exec -- dune utop . -- -emacs")
  ;; (setq utop-command "opam config exec -- dune utop . -- -emacs")

  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))
(lp-emacs-elpa-package 'dune)

;;; structured parenthesis editing, paredit replacement
;; (lp-emacs-elpa-package 'puni
;;   (setq lp--puni-mode-hooks
;; 	'(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook))

;;   (dolist (hook lp--puni-mode-hooks)
;;     (add-hook hook #'puni-mode))

;;   (define-key puni-mode-map [remap puni-kill-active-region] #'kill-region)
;;   (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;;; lisps and schemes and racket
(lp-emacs-elpa-package 'sly
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(lp-emacs-elpa-package 'sly-asdf)

(lp-emacs-elpa-package 'sly-quicklisp)

;; eldoc provides minibuffer hints for elisp things. it's super nice
(lp-emacs-elpa-package 'eldoc
  (global-eldoc-mode 1))

(lp-emacs-elpa-package 'rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(lp-emacs-elpa-package 'paredit)
(lp-emacs-elpa-package 'geiser
  (add-hook 'geiser-mode-hook #'paredit-mode)
  (setq auto-mode-alist (cons '("\\.scm" . scheme-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
  (require 'geiser-mode)
  (define-key geiser-mode-map (kbd "C-.") nil) ; embark

  (setq geiser-scheme-implementation 'chez))

(lp-emacs-elpa-package 'geiser-chez
  (define-key geiser-mode-map (kbd "C-.") nil) ; embark
)


(lp-emacs-elpa-package 'racket-mode
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (lp-emacs-elpa-package 'flymake-racket
    (flymake-racket-setup)))


;;; symbolic computation
(lp-emacs-builtin-package 'calc
  ;; TODO types for bytes and stuff
  (setq math-additional-units '((GiB "1024 * MiB" "Giga Byte")
                                (MiB "1024 * KiB" "Mega Byte")
                                (KiB "1024 * B" "Kilo Byte")
                                (B nil "Byte")
                                (Gib "1024 * Mib" "Giga Bit")
                                (Mib "1024 * Kib" "Mega Bit")
                                (Kib "1024 * b" "Kilo Bit")
                                (b nil "B/8" "bit")))
  (setq math-units-table nil) ; clear the units table cache
  )

(lp-emacs-elpa-package 'maxima
  (add-hook 'maxima-mode-hook #'maxima-hook-function)
  (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
  (setq maxima-display-maxima-buffer t)
  (setq auto-mode-alist (cons '("\\.mac" . maxima-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.mc" . maxima-mode) auto-mode-alist)))

;;; docker
(lp-emacs-elpa-package 'dockerfile-mode)

;; package deprecated, use internal package tramp-container
(if (>= emacs-major-version 29)
    (lp-emacs-builtin-package 'tramp-container)
  (lp-emacs-elpa-package 'docker-tramp))

;;; eglot and language servers
(lp-emacs-elpa-package 'eglot
  (defvar eglot-prog-mode-hooks
    '(cc-mode-hook c-mode-hook c++-mode-hook cuda-mode-hook
                   rjsx-mode-hook python-mode-hook java-mode-hook
                   julia-mode-hook))
  (dolist (mode eglot-prog-mode-hooks)
    (add-hook mode 'eglot-ensure))

  (define-key eglot-mode-map (kbd "C-c y r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c y o") #'eglot-reconnect)
  (define-key eglot-mode-map (kbd "C-c y a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c y b") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c y h") #'eldoc)

  (setq eglot-extend-to-xref t) ;; Allow eglot in corss-referenced

  ;; non-project files its easy to use other servers, but pylsp has the
  ;; least problems imo
  (add-to-list 'eglot-server-programs '(python-mode "pylsp"))
  )

(lp-emacs-elpa-package 'eglot-java
  ;; (setcdr (assq 'java-mode eglot-server-programs) (list
  ;; (expand-file-name "~/.emacs.d/share/eclipse.jdt.ls/bin/jdtls")))
  (add-hook 'java-mode-hook #'eglot-ensure)
  (add-hook 'java-mode-hook #'eglot-java-mode)
  )

;; for the crashes https://github.com/non-Jedi/eglot-jl/issues/30
;; https://discourse.julialang.org/t/emacs-based-workflow/19400/73
(lp-emacs-elpa-package 'eglot-jl

  ;; Best way to do this: set this variable in a `.dir-locals.el' file
  ;; pointing to the directory where your Project.toml is. It should
  ;; have both LanguageServer and SymbolServer installed. For more info,
  ;; https://pkgdocs.julialang.org/v1/environments/
  (setq eglot-jl-language-server-project (expand-file-name "~/.julia/environments/v1.9"))
  (setq eglot-connect-timeout 5000)
  (setq julia-repl-switches "-p 4")
  (add-hook 'julia-mode-hook 'eglot-jl-init))

(lp-emacs-elpa-package 'consult-eglot
  (define-key eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))

(provide 'lp-programming)

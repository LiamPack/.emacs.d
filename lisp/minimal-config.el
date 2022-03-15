;;;
;; A minimal, low-tech, built-in-only configuration. Used with emacs 24 (!!) since sometimes I
;; develop on older systems.

;; Any documentation or commentary for a given package will be in [[config.org][my modern config]]. Except
;; maybe IDO. Since this is incredibly vanilla and has no commentary, it won't be a literate
;; configuration.
;;
;; To use, just (require 'config-minimal)
;;;

;; hacky way to remove the =custom-variables= generation in =init.el=
(setq custom-file (make-temp-file "emacs-custom-"))

;; some from https://pastebin.com/MDagsZD7
(lp-emacs-builtin-package 'emacs
  ;; why do these binds exist
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  ;; minimizes GC interference with user activity
  (setq gc-cons-threshold (* 16 1024 1024))

  ;; i fat-finger way too much to not have the confirmation
  (setq confirm-kill-emacs #'yes-or-no-p)
  ;;    (global-hl-line-modea t)               ; highlight current line
  (setq-default fill-column 92)         ; column length
  (column-number-mode t)                ; show column number in the mode line

  ;; name on top of frame
  (setq-default frame-title-format '("%b [%m]"))

  (setq warning-minimum-level :error)   ; avoid warning buffer

  ;; scroll
  (setq auto-window-vscroll nil)  ; avoid next-line to trigger line-move-partial
  (setq scroll-conservatively 10)
  (setq scroll-margin 7)
  (setq scroll-step 0)                  ; see info of `scroll-step`: as long as `scroll-conservatively`
                                        ; is a "large value" this should be fine
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'set-horizontal-scroll-bar-mode)
    (set-horizontal-scroll-bar-mode nil))

  (setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
  (setq mouse-wheel-follow-mouse 't)

  ;; other basiscs
  (setq ring-bell-function 'ignore)
  (setq inhibit-startup-screen t)

  ;; create backups in separate folder
  (setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
  (setq create-lockfiles nil)

  ;; answering just 'y' or 'n' will do
  (defalias 'yes-or-no-p 'y-or-n-p)

  (if (display-graphic-p)
      (blink-cursor-mode 1)
    (progn
      (blink-cursor-mode -1)
      (setq visible-cursor t)))

  (setq-default
   initial-scratch-message ";; Present Day
  "
   visible-bell t
   create-lockfiles nil)

  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)

  (setq indent-tabs-mode nil                 ; don't insert tab when indent
        help-window-select t                 ; focus on help window when openend
        window-combination-resize nil)       ; i'd rather balance than have it auto-proportionally
                                        ; balanace

  (defun lp--clean-up-buffer-or-region ()
    "Untabifies, indents and deletes trailing whitespace from buffer or region."
    (interactive)
    (save-excursion
      (unless (region-active-p)
        (mark-whole-buffer))
      (untabify (region-beginning) (region-end))
      (indent-region (region-beginning) (region-end))
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (delete-trailing-whitespace))))

  (define-key global-map (kbd "C-x k") #'(lambda () (interactive) (kill-buffer nil)))
  (define-key global-map (kbd "C-x K") #'(lambda () (interactive) (kill-buffer nil) (delete-window)))
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)
  (define-key global-map (kbd "C-c n") #'lp--clean-up-buffer-or-region)
  (define-key global-map (kbd "<f5>")  #'revert-buffer)

  (define-key global-map (kbd "M-z") #'zap-up-to-char) ;; i generally go up to a char non-inclusive
  (define-key global-map (kbd "M-Z") #'zap-to-char)

  (define-key global-map (kbd "C-<tab>") 'next-window-any-frame)
  (define-key global-map (kbd "<backtab>") 'previous-window-any-frame)
  (define-key global-map (kbd "C-x C-M-e") 'pp-macroexpand-last-sexp)
  ;; (define-key global-map (kbd "C-x C-e") 'eval-defun) ;; already bound to C-M-x
  (define-key global-map (kbd "C-h j") 'describe-keymap)
  (define-key global-map (kbd "C-c C-j") 'join-line)
  (define-key global-map (kbd "C-S-p") #'(lambda () (interactive) (previous-line 7)))
  (define-key global-map (kbd "C-S-n") #'(lambda () (interactive) (next-line 7)))
  )

(lp-emacs-builtin-package 'repeat
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)
  (when (> emacs-major-version 26)
    (repeat-mode 1)))

(let (window)

  (define-key global-map (kbd "M-o") 'other-window)
  (define-key global-map (kbd "M-O") (lambda () (interactive) (other-window -1)))

  ;; Thank you prot (see
  ;; https://protesilaos.com/dotemacs/#h:c110e399-3f43-4555-8427-b1afe44c0779)
  (define-key global-map (kbd "C-x C-o") 'display-buffer)
  ;; [2021-12-21 Tue] C-x C-{p,n} go to next/previous buffer (new to me)
  (setq display-buffer-alist
        `(
          ;; below current window
          ("\\*\\(e?shell\\|v?term\\|.*geiser.*\\|\\)\\*"
           (display-buffer-below-selected)
           (window-height . 0.3))
          ("\\*Org Agenda\\*"
           (display-buffer-reuse-window display-buffer-same-window))
          (".*eww.*"
           (display-buffer-reuse-window display-buffer-same-window))
          ("\\*Org Src.*"
           (display-buffer-reuse-window display-buffer-same-window)
           (window-height . fit-window-to-buffer))
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ;; top side window
          ("\\*\\(Flymake diagnostics\\|Package-Lint\\|flycheck\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0))
          ("\\*Messages.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\|compilation\\|\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2))
          ;; left side window
          ("\\*\\(.* # Help.*\\|Help\\)\\*" ; See the hooks for `visual-line-mode'
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . left)
           (slot . 0))
          ;; bottom buffer (NOT side window)
          ("\\*Embark Actions\\*"
           ( display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Embark\\)?.*Completions.*"
           ( display-buffer-at-bottom)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Output\\|Register Preview\\).*"
           ( display-buffer-at-bottom))

          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           ( display-buffer-below-selected)
           ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
           ;; the height is not known in advance.
           (window-height . 0.4))
          ("magit: .*"
           ( display-buffer-below-selected)
           (window-height . 0.4))
          ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
           ( display-buffer-below-selected)
           (window-height . fit-window-to-buffer))))

  (defvar resize-window-repeat-map
    (let ((map (make-sparse-keymap)))
      ;; Standard keys:
      (define-key map "^" 'enlarge-window)
      (define-key map "}" 'enlarge-window-horizontally)
      (define-key map "{" 'shrink-window-horizontally) ; prot note: those three are C-x KEY
      ;; Additional keys:
      (define-key map "v" 'shrink-window) ; prot note: this is not bound by default
      map)
    "Keymap to repeat window resizing commands.  Used in `repeat-mode'.")
  (put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
  (put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
  (put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
  (put 'shrink-window 'repeat-map 'resize-window-repeat-map)

  (let ((map global-map))
    (define-key map (kbd "C-x <down>") #'next-buffer)
    (define-key map (kbd "C-x <up>") #'previous-buffer)
    (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
    (define-key map (kbd "C-`") #'next-buffer)
    (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
    (define-key map (kbd "C-~") #'previous-buffer)
    (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
    (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
    (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "C-x }") #'enlarge-window)
    (define-key map (kbd "C-x {") #'shrink-window)
    (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
    (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)

  (lp-emacs-builtin-package 'winner
    (winner-mode t)     ; move between windows configuration
    ))

(lp-emacs-builtin-package 'time
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("America/New_York" "New York (USA)")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ))
  (setq display-time-default-load-average 0
        display-time-use-mail-icon t
        display-time-24hr-format t
        display-time-day-and-date t)

  (display-time-mode 1))

(lp-emacs-builtin-package 'man
  (define-key Man-mode-map (kbd "i") #'Man-goto-section)
  (define-key Man-mode-map (kbd "g") #'Man-update-manpage))

(lp-emacs-builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

(lp-emacs-builtin-package 'desktop
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path `(,user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save ".*")
  (setq desktop-buffers-not-to-save ".*")
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

(lp-emacs-builtin-package 'saveplace
  ;; :diminish
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)

  (if (version<= emacs-version "25.0")
      (toggle-save-place-globally)
    (save-place-mode 1))
  )

(load-theme 'manoj-dark)

(let (isearch)
  ;; :diminish
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)

  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-suffix-format " {%s/%s}")
  (setq lazy-count-prefix-format nil)
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)


  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    ;; (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "C-g") #'isearch-abort)
    (define-key map (kbd "M-/") #'isearch-complete))

  (define-key global-map (kbd "M-s M-o") 'multi-occur)
  (define-key occur-mode-map (kbd "t") 'toggle-truncate-lines)
  (add-hook 'occur-mode-hook #'(lambda () (interactive) (toggle-truncate-lines t)))
  (add-hook 'occur-mode-hook #'hl-line-mode)

  (setq list-matching-lines-jump-to-current-line t))

(lp-emacs-builtin-package 'ido
  (ido-mode +1)
  (ido-everywhere +1)

  (setq ido-use-filename-at-point 'guess) ; for find-file-at-point
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t) ; for recentf stuff
  (setq ido-show-dot-for-dired t) ; idk why not

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

  ;; more sensible cycling keybinds
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

  ;; M-x support for ido
  (add-hook 'ido-setup-hook 'ido-define-keys)
  (global-set-key
   "\M-x"
   (lambda ()
     (interactive)
     (call-interactively
      (intern
       (ido-completing-read
        "M-x "
        (all-completions "" obarray 'commandp))))))
  )

(lp-emacs-builtin-package 'minibuffer

  (setq completion-show-inline-help t)
  (setq completions-detailed t)
  (setq completion-ignore-case t)

  ;; always allow tab cycle, except if you're running `completion-at-point', in which case
  ;; we want to always allow completion to help us. If we invoke `c-a-p', then set the
  ;; threshold to `nil' in the current buffer.
  (setq completion-cycle-threshold 3)
  (defun lp--no-cycle-for-completion-at-point (&rest args)
    "Set `completion-cycle-threshold' to NIL for the current buffer
if we invoke `completion-at-point'. `completion-cycle-threshold'
will retain its original value in any invoked minibuffer commands
since the value is set locally."
    (setq-local completion-cycle-threshold nil))
  ;; emacs28 completion stuff
  (setq completions-group t)
  (setq completions-group-sort nil)


  (setq enable-recursive-minibuffers t)
  (require 'minibuf-eldef)
  (setq minibuffer-eldef-shorten-default t) ;; default completion in [bracks]

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq resize-mini-windows t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1) ;; update default completion if change

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  ;; (defun crm-indicator (args)
  ;;   (cons (concat "[CRM] " (car args)) (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq suggest-key-bindings t))

     ;;; Minibuffer history
(lp-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

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

(define-key global-map (kbd "M-,") 'pop-tag-mark) ;; for emacs 24 lmao

(when (version<= "25.0" emacs-version)
  (lp-emacs-builtin-package 'xref
    ;; All those have been changed for Emacs 28
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
    (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
    (setq xref-file-name-display 'project-relative)
    (setq xref-search-program 'ripgrep)
    ))

(when (< emacs-major-version 25)
  ;; taken from emacs25+
  (defmacro if-let (spec then &rest else)
    "Bind variables according to SPEC and evaluate THEN or ELSE.
Evaluate each binding in turn, as in `let*', stopping if a
binding value is nil.  If all are non-nil return the value of
THEN, otherwise the last form in ELSE.

Each element of SPEC is a list (SYMBOL VALUEFORM) that binds
SYMBOL to the value of VALUEFORM.  An element can additionally be
of the form (VALUEFORM), which is evaluated and checked for nil;
i.e. SYMBOL can be omitted if only the test result is of
interest.  It can also be of the form SYMBOL, then the binding of
SYMBOL is checked for nil.

As a special case, interprets a SPEC of the form \(SYMBOL SOMETHING)
like \((SYMBOL SOMETHING)).  This exists for backward compatibility
with an old syntax that accepted only one binding."
    (declare (indent 2)
             (debug ([&or (symbolp form)  ; must be first, Bug#48489
                          (&rest [&or symbolp (symbolp form) (form)])]
                     body)))
    (when (and (<= (length spec) 2)
               (not (listp (car spec))))
      ;; Adjust the single binding case
      (setq spec (list spec)))
    (list 'if-let* spec then (macroexp-progn else))))


(require 'calendar)
(setq-local calendar-date-display-form calendar-iso-date-display-form)
(require 'lp-scratch)
(setq lp--journal-dir (file-truename "~/org/roam/daily/"))
(setq lp--notes-dir (file-truename "~/org/roam/"))
(define-key global-map (kbd "C-c f j j") 'lp-journal-visit-today)
(define-key global-map (kbd "C-c f j n") 'lp-journal-visit-forward-one-day)
(define-key global-map (kbd "C-c f j p") 'lp-journal-visit-backward-one-day)

(define-key global-map (kbd "C-c f d") 'lp-notes-dired)
(define-key global-map (kbd "C-c f f") 'lp-notes-find-file)
(define-key global-map (kbd "C-c f n") 'lp-notes-make-file)

(let (prog-mode)
  ;; Mark TODOs , FIXME, BUG as red in src code
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face prepend))))))

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
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))

  (add-hook 'prog-mode-hook 'flymake-mode)

  (let ((map prog-mode-map))
    (define-key map (kbd "C-c y s") #'flymake-start)
    (define-key map (kbd "C-c y d") #'flymake-show-buffer-diagnostics) ; Emacs28
    (define-key map (kbd "C-c y n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c y p") #'flymake-goto-prev-error)))

(lp-emacs-elpa-package 'python
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "ipython"
        python-shell-prompt-detect-failure-warning nil)
  )

(lp-emacs-builtin-package 'cc-mode
  (setq gdb-many-windows 't)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq-default indent-tabs-mode nil)

  (define-key c-mode-map (kbd "C-j") 'c-indent-new-comment-line)
  (define-key c++-mode-map (kbd "C-j") 'c-indent-new-comment-line)
  (add-hook 'c++-mode-hook
            #'(lambda ()
                (setq compile-command "cmake .. -DCMAKE_EXRORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug; make clean; cmake --build . -j8")
                ))
  (define-key c-mode-map (kbd "C-M-q") nil)
  (define-key c-mode-map (kbd "C-M-q") nil)
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

(lp-emacs-builtin-package 'calc)

(lp-emacs-builtin-package 'vc
  (setq vc-handled-backends '(SVN Git))
  ;;;  As always, from Prot. Directly copied. No shame. See https://protesilaos.com/emacs/dotemacs#h:31deeff4-dfae-48d9-a906-1f3272f29bc9

  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)

  ;; Note that `prot-vc-git-setup-mode' will run the following when
  ;; activated:
  ;;
  ;;   (remove-hook 'log-edit-hook #'log-edit-show-files)
  ;;
  ;; If you need the window to pop back up, do it manually with C-c C-f
  ;; which calls `log-edit-show-files'.

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  ;; (setq add-log-mailing-address "info@protesilaos.com")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (add-hook 'log-view-mode-hook #'hl-line-mode)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch
    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
    (define-key map (kbd "C-x v o") #'vc-log-outgoing)
    (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
    (define-key map (kbd "C-x v d") #'vc-diff))
  (let ((map vc-dir-mode-map))
    (define-key map (kbd "b") #'vc-retrieve-tag)
    (define-key map (kbd "t") #'vc-create-tag)
    (define-key map (kbd "O") #'vc-log-outgoing)
    (define-key map (kbd "o") #'vc-dir-find-file-other-window)
    (define-key map (kbd "f") #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
    (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
    (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
    (define-key map (kbd "k") #'vc-dir-clean-files)
    (define-key map (kbd "G") #'vc-revert)
    (let ((prot-vc-git-branch-map (make-sparse-keymap)))
      (define-key map "B" prot-vc-git-branch-map)
      (define-key prot-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
      (define-key prot-vc-git-branch-map "s" #'vc-retrieve-tag) ; switch branch/tag
      ;; (define-key prot-vc-git-branch-map "c" #'prot-vc-git-checkout-remote)
                                        ; "checkout" remote
      (define-key prot-vc-git-branch-map "l" #'vc-print-branch-log)))
  (let ((map vc-annotate-mode-map))
    (define-key map (kbd "M-q") #'vc-annotate-toggle-annotation-visibility)
    (define-key map (kbd "C-c C-c") #'vc-annotate-goto-line)
    (define-key map (kbd "<return>") #'vc-annotate-find-revision-at-line))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
    (define-key map (kbd "<return>") #'log-view-find-revision)
    (define-key map (kbd "s") #'vc-log-search)
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming)
    (define-key map (kbd "F") #'vc-update)
    (define-key map (kbd "P") #'vc-push))


  )

(lp-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

(lp-emacs-builtin-package 'eshell
  (require 'esh-mode)
  (require 'esh-module)
  (setq eshell-modules-list             ; It works but may need review
        '(eshell-alias                  ; aliases
          eshell-basic
          eshell-cmpl                   ; tab completion
          eshell-dirs                   ; view the ring with `cd =`
          eshell-glob ; unix-style globbing (with recursive (**), not (~), {zero,one}-or-more (#, ##), ...)
          eshell-hist ; unix-style history (!ls, !?ls, ...)
          eshell-ls   ; ls
          eshell-pred ; zsh-like argument predication (see its man page or zsh examples)
          eshell-prompt       ; prompt navigation
          eshell-script       ; running `eshell` script files (source, ./, ...)
          eshell-term         ; for visual programs (vi, vim, top, ...)
          eshell-unix))       ; standard unix commands
  (setenv "PAGER" "cat")      ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)

  (setq eshell-buffer-shorthand t
        eshell-destroy-buffer-when-process-dies t)
  (global-set-key (kbd "<f1>") 'eshell))

(lp-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq prcoed-filter 'user))

(lp-emacs-builtin-package 'dired
  ;; disable ls by default
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (add-hook 'dired-mode-hook #'hl-line-mode))

(lp-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action))
  )
(lp-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

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
                         ;; And all other kinds of boring files
                         )))

(lp-emacs-builtin-package 'tramp
  ;;; I'm actually going to give scp a try as the default method for now. There might be benefits due to the encryption overhead required of ssh in =external= use cases.

  ;; ssh > scp (the default).
  ;; (setq tramp-default-method "ssh")
  ;; (setq tramp-default-user "packell1")

  (setq tramp-verbose 3) ;; can go up to 11! 3 is the default.
  )

(provide 'minimal-config)

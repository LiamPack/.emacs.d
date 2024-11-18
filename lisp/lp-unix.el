;;; process communication backend
(lp-emacs-builtin-package 'comint
  (setq comint-delete-old-input t)
  (setq comint-eol-on-send t)
  (setq comint-history-isearch nil) ;; Separate `C-r' and `C-M-r'
  (setq comint-input-ring-size 500)
  (setq comint-move-point-for-output nil) ;; Don't move point for new output
  (setq comint-prompt-read-only t)
  (setq comint-use-prompt-regexp nil)
  (setq comint-completion-recexact t)
  (setq comint-buffer-maximum-size 9999)
  )

;;; shells
(lp-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq shell-kill-buffer-on-exit t)
  (setq ansi-color-for-comint-mode t))
;;; TODO: perosnalize.
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
          eshell-tramp        ; u kno
          eshell-unix))       ; standard unix commands
  (setenv "PAGER" "cat")      ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)

  (setq eshell-buffer-shorthand t
        eshell-destroy-buffer-when-process-dies t)
  (global-set-key (kbd "<f1>") 'eshell))


;;; directory editing
;;; TODO: personalize, check manual and settings.
(lp-emacs-builtin-package 'dired
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1)))

  ;; disable ls by default
  (setq delete-by-moving-to-trash t)
  (setq insert-directory-program "ls"
        dired-use-ls-dired t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
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
    (define-key map (kbd "M-s f") #'consult-find)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action)))

(lp-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))


;;; "write"-grep -- allow editing of grep-like buffers
(lp-emacs-elpa-package 'wgrep
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

;;; diffing
(lp-emacs-elpa-package 'ediff
  ;; :diminish ediff-mode
  (setq ediff-diff-options "-w"))


;;; A top-like package for emcas
(lp-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 2)
  (setq proced-descend t)
  (setq prcoed-filter 'user))

;;; remote editing
(lp-emacs-builtin-package 'tramp
    ;;; I'm actually going to give scp a try as the default method for
    ;;; now. There might be benefits due to the encryption overhead
    ;;; required of ssh in =external= use cases.

  ;; ssh > scp (the default).
  ;; (setq tramp-default-method "ssh")
  ;; (setq tramp-default-user "packell1")

  (setq tramp-verbose 3) ;; can go up to 11! 3 is the default.
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=localhost:10"))
  )

(provide 'lp-unix)

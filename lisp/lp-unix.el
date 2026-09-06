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
  (setq comint-buffer-maximum-size 9999))

;;; shells
(lp-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t)
  (setq shell-kill-buffer-on-exit t)
  (setq ansi-color-for-comint-mode t))

(lp-emacs-builtin-package 'eshell
  (setenv "PAGER" "cat")      ; solves issues, such as with 'git log' and the default 'less'
  (setq eshell-cd-on-directory t)

  ;; ssh & tramp configuration for eshell
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
  (setq dired-auto-revert-buffer t)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

(lp-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'consult-find)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action)))

(lp-emacs-builtin-package 'dired-x
  (define-key dired-mode-map (kbd "I") #'dired-info))

(lp-emacs-elpa-package 'dired-subtree
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>")  #'dired-subtree-toggle)
    (define-key map (kbd "TAB")  #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>")  #'dired-subtree-remove)
    (define-key map (kbd "S-TAB")  #'dired-subtree-remove))

  (setq dired-subtree-use-backgrounds nil))

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
               (format "DISPLAY=localhost:10")))

(provide 'lp-unix)

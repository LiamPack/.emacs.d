(require 'use-package)

;; Mark TODOs , FIXME, BUG as red in src code
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face prepend)))))

;;; Magit
;; God bless magit and all that it does
(use-package magit
  :straight t
  :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
         ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t)

  :bind ("C-x g" . magit-status))


;; More info here: [[https://github.com/syohex/emacs-git-gutter]]
(use-package git-gutter ; TODO - git gutter keybinds, going to different hunks and staging only certain portions!
  :straight t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package aggressive-indent
  :straight t
  :disabled t
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook css-mode-hook c-mode-hook c++-mode-hook sh-mode-hook) . aggressive-indent-mode))

(use-package poporg ; pop-out org mode window to edit comments. opposite of the embedding of source blocks
  :bind (("C-c /" . poporg-dwim)))

(provide 'prog-env)

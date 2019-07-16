(use-package helm
  :ensure t
  :disabled t
  :config
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)

  ;; helm is a little much for me
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)

  (helm-mode 1)

  ;; going to want to use M-x with helm's powerful interface
  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; We also are going to want to use the kill-ring feature
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; helm also has a great interface to a number of
  ;; different buffers and stuff like that
  (global-set-key (kbd "C-x b") 'helm-mini)

  ;; Another place to stick helm in. fuzzy matching,
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  ;; We also want helm-occur to not be on a horrible keybind
  (global-set-key (kbd "C-c h o") 'helm-occur)

  ;; REGISTERS!
  (global-set-key (kbd "C-c h x") 'helm-register)
  (with-eval-after-load 'helm
    (setq helm-always-two-windows nil)
    (setq helm-display-buffer-default-height 15)
    (setq helm-default-display-buffer-functions '(display-buffer-in-side-window)))
  )

(provide 'use-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme stuff
;; The deeper blue theme is loaded but the resulting text
;; appears black in Aquamacs. This can be fixed by setting
;; the font color under Menu Bar->Options->Appearance->Font For...
;; and then setting "Adopt Face and Frame Parameter as Frame Default"
;; (use-package sourcerer-theme
;;   :ensure t)

;; (set-face-background 'hl-line "#372E2D")
;; ;; The minibuffer default colors with my theme are impossible to read, so change
;; ;; them to something better using ivy-minibuffer-match-face.
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((type tty) (background dark)) (:background "nil"))))
;;  '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
;;  '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
;;  '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
;;  '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
;;  '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
;;  '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
;;  '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
;;  '(header-line ((t (:background "#003366"))))
;;  '(ivy-minibuffer-match-face-1 ((((class color) (background light)) (:background "#555555")) (((class color) (background dark)) (:background "#555555"))))
;;  '(ivy-minibuffer-match-face-2 ((t (:background "#314f30" :weight bold))))
;;  '(ivy-minibuffer-match-face-3 ((t (:background "#48225b" :weight bold))))
;;  '(ivy-minibuffer-match-face-4 ((t (:background "#680a0a" :weight bold))))
;;  '(which-func ((t (:foreground "#8fb28f")))))



;; get something up there for header
(which-function-mode t)

;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))

(defmacro with-face
    (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format
        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]")))
              )
        )
  )
;; Call the header line update
;; (add-hook 'buffer-list-update-hook
;;           'sl/display-header)
(provide 'use-header)

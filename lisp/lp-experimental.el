(lp-emacs-elpa-package 'simple-httpd)
(lp-emacs-elpa-package 'f)
(lp-emacs-elpa-package 's)
(lp-emacs-elpa-package 'dash)
(lp-emacs-elpa-package 'async)

(lp-emacs-elpa-package 'org-ql) ;; for updating note molds via org export

(lp-emacs-elpa-package 'gptel ; christ almighty am i actually using this
 ;Available models
  (setq gptel-model "mistral-7b-openorca.Q4_0.gguf") ;Pick your default model
  (setq gptel-backend (gptel-make-gpt4all "GPT4All"
						  :protocol "http"
						  :host "localhost:4891"                 ;Where it's running
						  :models '("mistral-7b-openorca.Q4_0.gguf"))) ;Available models
  (setq-default gptel-max-tokens 500))

(provide 'lp-experimental)

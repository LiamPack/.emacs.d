(require 'use-package)

(use-package orderless
  :straight t
  :demand
  :config
  ;;(setq orderless-component-separator " +")
  (setq completion-styles '(orderless))
  (setq  completion-category-defaults nil
         completion-category-overrides '((file (styles . (partial-completion)))))
  (setq orderless-matching-styles '(orderless-prefixes
                                    orderless-literal
                                    orderless-strict-leading-initialism
                                    orderless-regexp
                                    ;;orderless-flex
                                    ))

  ;;   (defun prot-orderless-literal-dispatcher (pattern _index _total)
  ;;     "Literal style dispatcher using the equals sign as a suffix.
  ;; It matches PATTERN _INDEX and _TOTAL according to how Orderless
  ;; parses its input."
  ;;     (when (string-suffix-p "=" pattern)
  ;;       `(orderless-literal . ,(substring pattern 0 -1))))

  ;;   (defun prot-orderless-initialism-dispatcher (pattern _index _total)
  ;;     "Leading initialism  dispatcher using the comma suffix.
  ;; It matches PATTERN _INDEX and _TOTAL according to how Orderless
  ;; parses its input."
  ;;     (when (string-suffix-p "," pattern)
  ;;       `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

  ;;   (setq orderless-style-dispatchers
  ;;         '(prot-orderless-literal-dispatcher
  ;;           prot-orderless-initialism-dispatcher))
  ;;(setq completion-styles '(orderless))
  ;; SPC should never complete: use it for `orderless' groups.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("?" . nil)))

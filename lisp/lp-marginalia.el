(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :demand
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))

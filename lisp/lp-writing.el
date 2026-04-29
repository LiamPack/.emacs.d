;;; TODO: paragraph-start tuning. what's a paragraph, and is it consistent with an outline expression?

(lp-emacs-builtin-package 'text-mode
  (add-hook 'text-mode-hook
            #'(lambda ()
                (interactive)
                (setq-local paragraph-start "\\|[ 	]*$\\|^[ ]*\\*\\|^[ ]*[1-9]+\\.\\|^[ ]*\\+\\|^x \\|^[ ]*-"))))

;; "\\|\\*\\| *-\\| *[1-9]\\.\\|[ 	]*$\\| *+"
(lp-emacs-builtin-package 'outline
  (setq outline-regexp "^= .+ =\n=+$\\|^- .+ -\n-+$\\|^[*]+")
  (setq outline-minor-mode-highlight t) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!

  (define-key global-map (kbd "<f10>") #'outline-minor-mode)
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer)
    )

  )

(lp-emacs-elpa-package 'markdown-mode)

(lp-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t))

(lp-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((text-mode .  "") ; just use the page-break to make things
			      ; simple
          (org-mode . "^\\*+ +")
          (t . ,(or outline-regexp logos--page-delimiter))))

  (setq-default logos-hide-mode-line t
                logos-hide-buffer-boundaries nil
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-scroll-lock t
                logos-olivetti t)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim) ; C-x ]
    (define-key map [remap backward-page] #'logos-backward-page-dwim) ; C-x [
    (define-key map (kbd "M-^") #'logos-focus-mode)
    ))

;;; style checking and evaluation
(lp-emacs-elpa-package 'smog)
(lp-emacs-elpa-package 'writegood-mode)


;;; spelling and spellchecking
(lp-emacs-builtin-package 'ispell
  (setq ispell-dictionary "english")
  (setq ispell-silently-savep t))

(lp-emacs-builtin-package 'flyspell
  ;; :diminish flyspell-mode
  (dolist (mode-hook '(org-mode-hook markdown-mode-hook))
    (add-hook mode-hook #'flyspell-mode))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (define-key flyspell-mode-map (kbd "C-;") nil) ;; gets in the way of things.
  (define-key flyspell-mode-map (kbd "C-M-i") nil) ;; gets in the way of things.
  )

;; prettify symbols...
(defvar my-latex-prettify-symbols-alist
  '(;; Greek lowercase
    ("\\alpha"        . ?α)
    ("\\beta"         . ?β)
    ("\\gamma"        . ?γ)
    ("\\delta"        . ?δ)
    ("\\epsilon"      . ?ε)
    ("\\varepsilon"   . ?ϵ)
    ("\\zeta"         . ?ζ)
    ("\\eta"          . ?η)
    ("\\theta"        . ?θ)
    ("\\vartheta"     . ?ϑ)
    ("\\iota"         . ?ι)
    ("\\kappa"        . ?κ)
    ("\\lambda"       . ?λ)
    ("\\mu"           . ?μ)
    ("\\nu"           . ?ν)
    ("\\xi"           . ?ξ)
    ("\\pi"           . ?π)
    ("\\varpi"        . ?ϖ)
    ("\\rho"          . ?ρ)
    ("\\varrho"       . ?ϱ)
    ("\\sigma"        . ?σ)
    ("\\varsigma"     . ?ς)
    ("\\tau"          . ?τ)
    ("\\upsilon"      . ?υ)
    ("\\phi"          . ?φ)
    ("\\varphi"       . ?ϕ)
    ("\\chi"          . ?χ)
    ("\\psi"          . ?ψ)
    ("\\omega"        . ?ω)

    ;; Greek uppercase
    ("\\Gamma"        . ?Γ)
    ("\\Delta"        . ?Δ)
    ("\\Theta"        . ?Θ)
    ("\\Lambda"       . ?Λ)
    ("\\Xi"           . ?Ξ)
    ("\\Pi"           . ?Π)
    ("\\Sigma"        . ?Σ)
    ("\\Upsilon"      . ?Υ)
    ("\\Phi"          . ?Φ)
    ("\\Psi"          . ?Ψ)
    ("\\Omega"        . ?Ω)

    ;; Blackboard / double-struck
    ("\\mathbb{N}"    . ?ℕ)
    ("\\mathbb{Z}"    . ?ℤ)
    ("\\mathbb{Q}"    . ?ℚ)
    ("\\mathbb{R}"    . ?ℝ)
    ("\\mathbb{C}"    . ?ℂ)
    ("\\mathbb{P}"    . ?ℙ)
    ("\\mathbb{E}"    . ?𝔼)
    ("\\mathbbm{1}"   . ?𝟙)
    ("\\mathds{1}"    . ?𝟙)
    ("\\mathbf{1}"    . ?𝟙)

    ;; Calculus / analysis
    ("\\partial"      . ?∂)
    ("\\nabla"        . ?∇)
    ("\\Box"          . ?□)
    ("\\square"       . ?□)
    ("\\int"          . ?∫)
    ("\\iint"         . ?∬)
    ("\\iiint"        . ?∭)
    ("\\oint"         . ?∮)
    ("\\sum"          . ?∑)
    ("\\prod"         . ?∏)
    ("\\coprod"       . ?∐)
    ("\\infty"        . ?∞)
    ("\\ell"          . ?ℓ)
    ("\\Re"           . ?ℜ)
    ("\\Im"           . ?ℑ)
    ("\\wp"           . ?℘)
    ("\\hbar"         . ?ℏ)
    ("\\mathrm{d}"    . ?𝑑)
    ("\\dd"           . ?𝑑)

    ;; Operators
    ("\\circ"         . ?∘)
    ("\\bullet"       . ?∙)
    ("\\cdot"         . ?·)
    ("\\times"        . ?×)
    ("\\otimes"       . ?⊗)
    ("\\oplus"        . ?⊕)
    ("\\ominus"       . ?⊖)
    ("\\oslash"       . ?⊘)
    ("\\odot"         . ?⊙)
    ("\\ast"          . ?∗)
    ("\\star"         . ?⋆)
    ("\\wedge"        . ?∧)
    ("\\vee"          . ?∨)
    ("\\land"         . ?∧)
    ("\\lor"          . ?∨)
    ("\\cap"          . ?∩)
    ("\\cup"          . ?∪)
    ("\\sqcap"        . ?⊓)
    ("\\sqcup"        . ?⊔)
    ("\\setminus"     . ?∖)
    ("\\wr"           . ?≀)

    ;; Equality / equivalence / approximation
    ("\\neq"          . ?≠)
    ("\\ne"           . ?≠)
    ("\\equiv"        . ?≡)
    ("\\cong"         . ?≅)
    ("\\simeq"        . ?≃)
    ("\\sim"          . ?∼)
    ("\\nsim"         . ?≁)
    ("\\approx"       . ?≈)
    ("\\asymp"        . ?≍)
    ("\\propto"       . ?∝)
    ("\\doteq"        . ?≐)
    ("\\triangleq"    . ?≜)
    ("\\coloneqq"     . ?≔)
    ("\\eqqcolon"     . ?≕)
    ("\\defeq"        . ?≔)

    ;; Order relations
    ("\\leq"          . ?≤)
    ("\\le"           . ?≤)
    ("\\geq"          . ?≥)
    ("\\ge"           . ?≥)
    ("\\ll"           . ?≪)
    ("\\gg"           . ?≫)
    ("\\prec"         . ?≺)
    ("\\preceq"       . ?≼)
    ("\\succ"         . ?≻)
    ("\\succeq"       . ?≽)
    ("\\lesssim"      . ?≲)
    ("\\gtrsim"       . ?≳)

    ;; Set theory
    ("\\in"           . ?∈)
    ("\\notin"        . ?∉)
    ("\\ni"           . ?∋)
    ("\\subset"       . ?⊂)
    ("\\supset"       . ?⊃)
    ("\\subseteq"     . ?⊆)
    ("\\supseteq"     . ?⊇)
    ("\\nsubseteq"    . ?⊈)
    ("\\nsupseteq"    . ?⊉)
    ("\\emptyset"     . ?∅)
    ("\\varnothing"   . ?∅)
    ("\\aleph"        . ?ℵ)

    ;; Logic
    ("\\forall"       . ?∀)
    ("\\exists"       . ?∃)
    ("\\nexists"      . ?∄)
    ("\\neg"          . ?¬)
    ("\\lnot"         . ?¬)
    ("\\top"          . ?⊤)
    ("\\bot"          . ?⊥)
    ("\\vdash"        . ?⊢)
    ("\\dashv"        . ?⊣)
    ("\\models"       . ?⊨)
    ("\\therefore"    . ?∴)
    ("\\because"      . ?∵)

    ;; Arrows
    ("\\to"           . ?→)
    ("\\rightarrow"   . ?→)
    ("\\leftarrow"    . ?←)
    ("\\mapsto"       . ?↦)
    ("\\longmapsto"   . ?⟼)
    ("\\Rightarrow"   . ?⇒)
    ("\\Leftarrow"    . ?⇐)
    ("\\Leftrightarrow" . ?⇔)
    ("\\leftrightarrow" . ?↔)
    ("\\longrightarrow" . ?⟶)
    ("\\longleftarrow"  . ?⟵)
    ("\\Longrightarrow" . ?⟹)
    ("\\Longleftarrow"  . ?⟸)
    ("\\Longleftrightarrow" . ?⟺)
    ("\\uparrow"      . ?↑)
    ("\\downarrow"    . ?↓)
    ("\\updownarrow"  . ?↕)
    ("\\Uparrow"      . ?⇑)
    ("\\Downarrow"    . ?⇓)
    ("\\Updownarrow"  . ?⇕)
    ("\\nearrow"      . ?↗)
    ("\\searrow"      . ?↘)
    ("\\swarrow"      . ?↙)
    ("\\nwarrow"      . ?↖)
    ("\\hookrightarrow" . ?↪)
    ("\\hookleftarrow"  . ?↩)

    ;; Probability / useful aliases
    ("\\Pr"           . ?ℙ)
    ("\\P"            . ?ℙ)
    ("\\E"            . ?𝔼)
    ("\\1"            . ?𝟙)
    ("\\ind"          . ?𝟙)
    ("\\perp"         . ?⊥)

    ;; Brackets and delimiters
    ("\\langle"       . ?⟨)
    ("\\rangle"       . ?⟩)
    ("\\lfloor"       . ?⌊)
    ("\\rfloor"       . ?⌋)
    ("\\lceil"        . ?⌈)
    ("\\rceil"        . ?⌉)
    ("\\Vert"         . ?‖)
    ("\\vert"         . ?|)

    ;; Miscellaneous
    ("\\pm"           . ?±)
    ("\\mp"           . ?∓)
    ("\\div"          . ?÷)
    ("\\sqrt"         . ?√)
    ("\\angle"        . ?∠)
    ("\\triangle"     . ?△)
    ("\\diamond"      . ?◇)
    ("\\sharp"        . ?♯)
    ("\\flat"         . ?♭)
    ("\\natural"      . ?♮)
    ("\\dagger"       . ?†)
    ("\\ddagger"      . ?‡)
    ("\\S"            . ?§)
    ("\\copyright"    . ?©)
    ("\\checkmark"    . ?✓)
    ("\\dots"         . ?…)
    ("\\ldots"        . ?…)
    ("\\cdots"        . ?⋯)
    ("\\vdots"        . ?⋮)
    ("\\ddots"        . ?⋱)

    ;; ASCII-style aliases
    ("->"             . ?→)
    ("<-"             . ?←)
    ("=>"             . ?⇒)
    ("<=>"            . ?⇔)
    ("<="             . ?≤)
    (">="             . ?≥)
    ("!="             . ?≠)
    ("=="             . ?≡)
    ("~="             . ?≈)
    ("..."            . ?…)))

(defun my-ascii-letter-p (ch)
  "Non-nil if CH is an ASCII letter."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z)))))

(defun my-latex-prettify-compose-p (start end _match)
  "Predicate for LaTeX-style `prettify-symbols-mode'.

Allows symbols after ordinary letters, as in d\\phi or e^{i\\theta}.
Rejects partial matches inside longer LaTeX control words, as in
\\subset inside \\subseteq."
  (let ((first (char-after start))
        (next  (char-after end)))
    (if (eq first ?\\)
        ;; For LaTeX commands, only protect the right boundary.
        ;; This allows d\phi, x\in A, e^{i\theta}, etc.
        (not (my-ascii-letter-p next))

      ;; For non-LaTeX ASCII aliases like ->, <=, !=, use the default rule
      ;; when available.
      (if (fboundp 'prettify-symbols-default-compose-p)
          (prettify-symbols-default-compose-p start end _match)
        t))))

(defun my-text-mode-prettify-latex ()
  (setq-local prettify-symbols-alist my-latex-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate
              #'my-latex-prettify-compose-p)
  (prettify-symbols-mode 1))

;; (add-hook 'text-mode-hook #'my-text-mode-prettify-latex)
(add-hook 'org-mode-hook  #'my-text-mode-prettify-latex)

(provide 'lp-writing)

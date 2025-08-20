
;; Ensure AUCTeX is installed
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . visual-line-mode)
         (DocView-mode . auto-revert-mode)
         (LaTeX-mode . flyspell-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-command-default "LatexMk"))

(use-package mixed-pitch
  :hook
  (LaTeX-mode . my/latex-setup))

(defun my/latex-setup ()
  "Enable variable-pitch only for free text in LaTeX."
  (mixed-pitch-mode 1)
  ;; Keep certain faces fixed-pitch
  (dolist (face '(font-latex-math-face
                  font-latex-verbatim-face
                  font-latex-warning-face
                  font-latex-sedate-face
                  font-latex-string-face
                  font-latex-sectioning-5-face
                  font-lock-comment-face
                  font-lock-function-name-face
                  font-lock-keyword-face
                  font-lock-builtin-face
                  font-lock-constant-face
                  font-lock-variable-name-face))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

(provide 'latex-conf)


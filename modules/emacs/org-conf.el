(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-hide-emphasis-markers t)

 (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    (add-hook 'org-mode-hook 'flyspell-mode))

 (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

 (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Helvetica"))))
   '(fixed-pitch ((t ( :family "Berkeley Mono"))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(org-drawer ((t (:inherit fixed-pitch :foreground "gray60")))))

 (add-hook 'org-mode-hook 'variable-pitch-mode)
 (add-hook 'org-mode-hook 'visual-line-mode)

(set-face-attribute 'org-level-1 nil :height 1.5)
(set-face-attribute 'org-level-2 nil :height 1.3)
(set-face-attribute 'org-level-3 nil :height 1.2)
(set-face-attribute 'org-level-4 nil :height 1.0)
(set-face-attribute 'org-document-title nil :height 1.8)
(setq org-src-fontify-natively t)
(setq org-latex-compiler "xelatex")
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1))

;; Scaling latex org mode

(defun my/org-latex-refresh-preview-scaling ()
  "Refresh Org LaTeX preview images to match current text scale."
  (when (eq major-mode 'org-mode)
    (setq org-format-latex-options
          (plist-put org-format-latex-options
                     :scale (+ 1 (* 0.5 (or text-scale-mode-amount 0)))))
    (org-clear-latex-preview)
    (org-latex-preview)))

(defun my/org-update-latex-on-scale (&rest _args)
  "Update LaTeX preview scaling when text is scaled."
  (when (eq major-mode 'org-mode)
    (my/org-latex-refresh-preview-scaling)))

(advice-add 'text-scale-increase :after #'my/org-update-latex-on-scale)
(advice-add 'text-scale-decrease :after #'my/org-update-latex-on-scale)

;;end

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell   . t)
   (haskell . t)))
(setq org-confirm-babel-evaluate nil)

;; Fallback for Unicode symbols

(global-set-key [mouse-3] 'flyspell-correct-word-before-point)

(provide 'org-conf)

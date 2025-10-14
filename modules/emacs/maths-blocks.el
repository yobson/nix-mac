(use-package org-special-block-extras
  :ensure t
  :after org
  :hook (org-mode . org-special-block-extras-mode))

(with-eval-after-load 'org
  (require 'org-special-block-extras)

  ;; Definition
  (font-lock-add-keywords
     'org-mode
     '(("^\\s-*#\\+begin_def\\b.*\\(\n\\(.\\|\n\\)*?\\)#\\+end_def\\b"
        (0 '(:foreground "orange" :weight bold :extend t)))))

  (org-defblock def (definition-name)
    [:face '(:foreground "orange" :weight bold) :display 'full])

  (org-defblock def (definition-name nil :backend html)
    (format "HTML %s: %s" definition-name contents))

  (org-defblock def (definition-name nil :backend latex)
    (format "Latex %s: %s" definition-name contents)))

(provide 'maths-blocks)

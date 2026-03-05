(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'markdown-conf)

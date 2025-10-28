; (load-theme 'monokai t)

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
  
(use-package auto-dark
  :custom
  (auto-dark-themes '((material) (material-light)))
  (auto-dark-allow-osascript t)
  :init (auto-dark-mode))

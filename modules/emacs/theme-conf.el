(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  (ar/load-material-org-tweaks)
  :init
  (defun ar/load-material-org-present-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "#2BA3FF"))

    (with-eval-after-load 'faces
      (set-face-attribute 'org-level-1 nil :foreground "#ff69b4" :background nil :box nil)
      (set-face-attribute 'org-level-2 nil :inherit 'lisp-extra-font-lock-quoted :foreground nil :background nil :box nil)
      (set-face-attribute 'org-block nil :background "grey11" :box nil)))

  (defun ar/drop-material-org-present-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "orange"))

    (with-eval-after-load 'faces
      (set-face-attribute 'org-level-1 nil :foreground nil :background nil :box nil)
      (set-face-attribute 'org-level-2 nil :inherit nil :foreground nil :background nil :box nil)
      (set-face-attribute 'org-block nil :background nil :box nil)))

  (defun ar/load-material-org-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "orange"))

    (with-eval-after-load 'faces
      (set-face-attribute 'header-line nil :background "#212121" :foreground "dark grey")
      ;; From https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c#file-customized-org-mode-theme-el
      (set-face-attribute 'default nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil
                          ;; :family "Menlo" ;; or Meslo if unavailable: https://github.com/andreberg/Meslo-Font
                          ;; :family "Hack" ;; brew tap homebrew/cask-fonts && brew cask install font-hack
                          :family "JetBrains Mono" ;; brew tap homebrew/cask-fonts && brew install --cask font-jetbrains-mono
                          ;; :family "mononoki" ;; https://madmalik.github.io/mononoki/ or sudo apt-get install fonts-mononoki
                          :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal
                          :width 'normal :foundry "nil")
      ;; Enable rendering SF symbols on macOS.
      (when (memq system-type '(darwin))
        (set-fontset-font t nil "SF Pro Display" nil 'append))

      ;; Emoji's: welcome back to Emacs
      ;; https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
      (when (>= emacs-major-version 27)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

      ;; Hardcode region theme color.
      (set-face-attribute 'region nil :background "#3f464c" :foreground "#eeeeec" :underline nil)
      (set-face-attribute 'mode-line nil :background "#191919" :box nil)

      ;; Styling moody https://github.com/tarsius/moody
      (let ((line (face-attribute 'mode-line :underline)))
        (set-face-attribute 'mode-line nil :overline   line)
        (set-face-attribute 'mode-line-inactive nil :overline   line)
        (set-face-attribute 'mode-line-inactive nil :underline  line)
        (set-face-attribute 'mode-line nil :box nil)
        (set-face-attribute 'mode-line-inactive nil :box nil)
        (set-face-attribute 'mode-line-inactive nil :background "#212121" :foreground "#5B6268")))

    (with-eval-after-load 'font-lock
      (set-face-attribute 'font-lock-constant-face nil :foreground "#C792EA")
      (set-face-attribute 'font-lock-keyword-face nil :foreground "#2BA3FF" :slant 'italic)
      (set-face-attribute 'font-lock-preprocessor-face nil :inherit 'bold :foreground "#2BA3FF" :slant 'italic :weight 'normal)
      (set-face-attribute 'font-lock-string-face nil :foreground "#C3E88D")
      (set-face-attribute 'font-lock-type-face nil :foreground "#FFCB6B")
      (set-face-attribute 'font-lock-variable-name-face nil :foreground "#FF5370"))

    (with-eval-after-load 'em-prompt
      (set-face-attribute 'eshell-prompt nil :foreground "#eeffff"))

    (with-eval-after-load 'company
      (set-face-attribute 'company-preview-search nil :foreground "sandy brown" :background nil)
      (set-face-attribute 'company-preview-common nil :inherit 'default :foreground nil :background "#212121"))

    (with-eval-after-load 'company-box
      (set-face-attribute 'company-box-candidate  nil :inherit 'default :foreground "#eeffff" :background "#212121" :box nil)
      (set-face-attribute 'company-box-background nil :inherit 'default :background "#212121" :box nil)
      (set-face-attribute 'company-box-annotation nil :inherit 'company-tooltip-annotation :background "#212121" :foreground "dim gray")
      (set-face-attribute 'company-box-selection nil :inherit 'company-tooltip-selection :foreground "sandy brown"))

    (with-eval-after-load 'popup
      (set-face-attribute 'popup-menu-face nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default))
      (set-face-attribute 'popup-menu-selection-face nil
                          :foreground "sandy brown"
                          :background "dim gray"))

    (with-eval-after-load 'paren
      (set-face-attribute 'show-paren-match nil
                          :background nil
                          :foreground "#FA009A"))

    (with-eval-after-load 'org-indent
      (set-face-attribute 'org-indent nil :background "#212121"))

    (with-eval-after-load 'org-faces
      (set-face-attribute 'org-hide nil :foreground "#212121" :background "#212121" :strike-through nil)
      (set-face-attribute 'org-done nil :foreground "#b9ccb2" :strike-through nil)
      (set-face-attribute 'org-agenda-date-today nil :foreground "#Fb1d84")
      (set-face-attribute 'org-agenda-done nil :foreground "#b9ccb2" :strike-through nil)
      (set-face-attribute 'org-table nil :background nil)
      (set-face-attribute 'org-code nil :background nil)
      (set-face-attribute 'org-level-1 nil :background nil :box nil)
      (set-face-attribute 'org-level-2 nil :background nil :box nil)
      (set-face-attribute 'org-level-3 nil :background nil :box nil)
      (set-face-attribute 'org-level-4 nil :background nil :box nil)
      (set-face-attribute 'org-level-5 nil :background nil :box nil)
      (set-face-attribute 'org-level-6 nil :background nil :box nil)
      (set-face-attribute 'org-level-7 nil :background nil :box nil)
      (set-face-attribute 'org-level-8 nil :background nil :box nil)
      (set-face-attribute 'org-block-begin-line nil :background nil :box nil)
      (set-face-attribute 'org-block-end-line nil :background nil :box nil)
      (set-face-attribute 'org-block nil :background nil :box nil))

    (with-eval-after-load 'mu4e-vars
      (set-face-attribute 'mu4e-header-highlight-face nil :inherit 'default :foreground "sandy brown" :weight 'bold :background nil)
      (set-face-attribute 'mu4e-unread-face nil :inherit 'default :weight 'bold :foreground "#2BA3FF" :underline nil))

    ;; No color for fringe, blends with the rest of the window.
    (with-eval-after-load 'fringe
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)))

    ;; No color for sp-pair-overlay-face.
    (with-eval-after-load 'smartparens
      (set-face-attribute 'sp-pair-overlay-face nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)))

    ;; Remove background so it doesn't look selected with region.
    ;; Make the foreground the same as `diredfl-flag-mark' (ie. orange).
    (with-eval-after-load 'diredfl
      (set-face-attribute 'diredfl-flag-mark-line nil
                          :foreground "orange"
                          :background nil))

    (with-eval-after-load 'dired-subtree
      (set-face-attribute 'dired-subtree-depth-1-face nil
                          :background nil)
      (set-face-attribute 'dired-subtree-depth-2-face nil
                          :background nil)
      (set-face-attribute 'dired-subtree-depth-3-face nil
                          :background nil)
      (set-face-attribute 'dired-subtree-depth-4-face nil
                          :background nil)
      (set-face-attribute 'dired-subtree-depth-5-face nil
                          :background nil)
      (set-face-attribute 'dired-subtree-depth-6-face nil
                          :background nil))

    ;; Trying out line underline (instead of wave).
    (mapatoms (lambda (atom)
                (let ((underline nil))
                  (when (and (facep atom)
                             (setq underline
                                   (face-attribute atom
                                                   :underline))
                             (eq (plist-get underline :style) 'wave))
                    (plist-put underline :style 'line)
                    (set-face-attribute atom nil
                                        :underline underline)))))))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)

  (setq-default mode-line-format
                '(" "
                  mode-line-front-space
                  mode-line-client
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  " " mode-line-modes
                  mode-line-end-spaces)))

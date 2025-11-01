(setenv "LANG" "en_GB.UTF-8")
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq-default truncate-lines t)
(setq mac-option-modifier 'meta)

(use-package exec-path-from-shell
             :if (memq window-system '(mac ns x))
             :config
             (exec-path-from-shell-initialize))  

(setq ispell-program-name "hunspell")
(setenv
  "DICPATH"
  (concat (getenv "HOME") "/.config/dictionaries"))

(setenv "DICTIONARY" "en_GB")
(setq ispell-local-dictionary "en_GB")

;; Evil Mode
;; (use-package evil
;;   :init
;;   (setq evil-respect-visual-line-mode t)
;;   :config
;;   (evil-mode 1)
;;   (setq evil-undo-system 'undo-redo)
;;   (setq evil-vsplit-window-right t))
;; 
;; ;; Optional evil plugin
;; (use-package evil-visual-mark-mode
;;   :after evil)

(use-package company
             :config
             (add-hook 'after-init-hook 'global-company-mode))

; (require 'tree-sitter)
; (require 'tree-sitter-langs)

(use-package haskell-mode)
(use-package nix-mode
             :mode "\\.nix\\'")

; (add-hook 'haskell-mode-hook #'tree-sitter-mode)
; (add-hook 'nix-mode-hook #'tree-sitter-mode)

(use-package eglot
             :ensure t
             :config
             (add-hook 'haskell-mode-hook 'eglot-ensure)
             :config
             (setq-default eglot-workspace-configuration
                           '((haskell
                               (plugin
                                 (stan
                                   (globalOn . :json-false))))))  ;; disable stan
             :custom
             (eglot-autoshutdown t)  ;; shutdown language server after closing last file
             (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
             )

(use-package ligature
             :load-path "path-to-ligature-repo"
             :config
             (ligature-set-ligatures
               'prog-mode
               '(; Group A
                 ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
                 ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
                 ; Group B
                 "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
                 "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
                 "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
                 "|=" "//=" "/="
                 ; Group C
                 "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
                 "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
                 "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
                 ; Group D
                 "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
                 ; Group E
                 "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
                 "{!--" "//" "///" "!!"
                 ; Group F
                 "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
                 ; Group G
                 "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
                 "--" "---"))
             ;; Enables ligature checks globally in all buffers. You can also do it
             ;; per mode with `ligature-mode'.
             (global-ligature-mode t))

(setq inhibit-startup-screen t)
(setq latex-preview-pane-use-frame nil) ; Optional: use same frame



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
  '(package-selected-packages nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:family "Berkeley Mono" :foundry "nil" :slant normal :weight regular :height 150 :width normal)))))

(transient-mark-mode 1)

;; Set default variable-pitch font globally
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 150)  ;; Replace with your preferred variable-pitch font

;; Set default fixed-pitch font globally
(set-face-attribute 'fixed-pitch nil :family "Berkeley Mono" :height 150)  ;; Replace with your preferred fixed-pitch font
(set-fontset-font t 'unicode "JuliaMono Nerd Font Mono" nil 'prepend)

(prefer-coding-system 'utf-8)

(use-package direnv
             :config
             (direnv-mode))


;; Enable basic mouse support in terminal
(xterm-mouse-mode 1)

(scroll-bar-mode -1)




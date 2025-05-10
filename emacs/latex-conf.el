;; (use-package tex
;;   :ensure auctex
;;   :defer t
;;   :hook ((LaTeX-mode . LaTeX-math-mode)
;;          (LaTeX-mode . turn-on-reftex)
;;          (LaTeX-mode . latex-preview-pane-mode)) ; enable preview pane in LaTeX mode
;;   :config
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         TeX-save-query nil
;;         TeX-PDF-mode t
;;         TeX-command-default "LatexMk")
;; 
;;   ;; Add latexmk to TeX-command-list if it's not already there
;;   (unless (assoc "LatexMk" TeX-command-list)
;;     (add-to-list 'TeX-command-list
;;                  '("LatexMk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk"))))
;; 
;; (use-package latex-preview-pane
;;   :hook (LaTeX-mode . latex-preview-pane-mode)
;;   :config
;;   (setq latex-preview-pane-multifile-mode 'auctex)
;; 
;;   ;; Always use main.tex as the file to compile
;;   (defun my-latex-preview-pane-compile ()
;;   (let* ((default-directory (locate-dominating-file buffer-file-name "main.tex"))
;;          (tex-file (expand-file-name "main.tex" default-directory))
;;          (auxdir (expand-file-name ".cache/latex" default-directory)))
;;     (setq latex-preview-pane-pdf-file (expand-file-name "main.pdf" default-directory))
;;     (start-process
;;      "latex-preview-pane"
;;      "*latex-preview-pane*"
;;      "latexmk" "-pdf" "-shell-escape" (concat "-auxdir=" auxdir) tex-file)
;;     (run-with-timer 0.1 nil #'latex-preview-pane-update)))
;; 
;;   (advice-add 'latex-preview-pane-update :override #'my-latex-preview-pane-compile))
;; 
;; 
;; (provide 'latex-conf)

;;; latex-conf.el --- LaTeX configuration -*- lexical-binding: t; -*-

;; AUCTeX and Preview Pane setup

;;; latex-conf.el --- LaTeX configuration -*- lexical-binding: t; -*-

;;; latex-conf.el --- LaTeX configuration -*- lexical-binding: t -*-

;; Ensure AUCTeX is installed
;; (use-package tex
;;   :ensure auctex
;;   :defer t
;;   :hook ((LaTeX-mode . LaTeX-math-mode)
;;          (LaTeX-mode . turn-on-reftex))
;;   :config
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         TeX-save-query nil
;;         TeX-PDF-mode t
;;         TeX-command-default "LatexMk")
;; 
;;   ;; Add LatexMk to TeX-command-list if not already there
;;   (unless (assoc "LatexMk" TeX-command-list)
;;     (add-to-list 'TeX-command-list
;;                  '("LatexMk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk"))))
;; 
;; ;; Latex Preview Pane config
;; (use-package latex-preview-pane
;;   :after tex
;;   :hook (LaTeX-mode . my/setup-latex-environment)
;;   :config
;;   (setq latex-preview-pane-multifile-mode 'auctex))
;; 
;; ;; --- Custom Functions ---
;; 
;; (defun my/latex-project-root ()
;;   "Find the project root by locating 'main.tex'."
;;   (locate-dominating-file buffer-file-name "main.tex"))
;; 
;; (defun my/latex-main-tex-file ()
;;   "Get the full path to the main.tex."
;;   (expand-file-name "main.tex" (my/latex-project-root)))
;; 
;; (defun my/latex-aux-directory ()
;;   "Return the aux directory relative to the project root."
;;   (expand-file-name ".cache/latex" (my/latex-project-root)))
;; 
;; (defun my/latex-pdf-file ()
;;   "Return the PDF file to preview."
;;   (expand-file-name "main.pdf" (my/latex-project-root)))
;; 
;; (defun my/latex-compile-main ()
;;   "Compile main.tex using latexmk with correct settings."
;;   (interactive)
;;   (let ((default-directory (my/latex-project-root)))
;;     (start-process
;;      "latex-preview-pane"
;;      "*latex-preview-pane*"
;;      "latexmk"
;;      "-pdf"
;;      "-shell-escape"
;;      (concat "-auxdir=" (my/latex-aux-directory))
;;      "main.tex")))
;; 
;; (defun my/setup-latex-environment ()
;;   "Setup LaTeX environment for multifile documents."
;;   (latex-preview-pane-mode 1)
;;   ;; Force TeX-master to always be main.tex
;;   (setq-local TeX-master (my/latex-main-tex-file))
;;   (setq-local latex-preview-pane-pdf-file (my/latex-pdf-file))
;;   (add-hook 'after-save-hook #'my/latex-compile-main nil t)
;;   ;; Optionally trigger initial compile immediately
;;   ;; (my/latex-compile-main)
;;   )
;; 
;; (provide 'latex-conf)
;; ;;; latex-conf.el ends here

;;; latex-conf.el --- LaTeX configuration -*- lexical-binding: t -*-

;; Ensure AUCTeX is installed
(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . flyspell-mode))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-command-default "LatexMk"))

  ;; Add LatexMk to TeX-command-list if not already there
  ;;(unless (assoc "LatexMk" TeX-command-list)
  ;;  (add-to-list 'TeX-command-list
  ;;               '("LatexMk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk"))))

;; Latex Preview Pane config
;;(use-package latex-preview-pane
;;  :after tex
;;  :hook (LaTeX-mode . my/setup-latex-environment)
;;  :config
;;  (setq latex-preview-pane-multifile-mode 'auctex
;;        latex-preview-pane-size 120)
;;
;;  ;; Automatically refresh the preview pane after compilation
;;  (defun my/latex-compile-main-and-refresh ()
;;    "Compile the LaTeX document and refresh the preview pane."
;;    (interactive)
;;    (let ((default-directory (my/latex-project-root)))
;;      (start-process
;;       "latex-preview-pane"
;;       "*latex-preview-pane*"
;;       "latexmk"
;;       "-pdf"
;;       "-shell-escape"
;;       (concat "-auxdir=" (my/latex-aux-directory))
;;       "main.tex"))
;;    ;; Wait for compilation to finish, then refresh preview and PDF buffer
;;    (add-hook 'TeX-after-compilation-finished-functions
;;              #'my/latex-refresh-preview))
;;
;;  (defun my/latex-refresh-preview (process)
;;    "Refresh the latex preview pane and PDF buffer after compile."
;;    (when (eq (process-status process) 'exit)
;;      (latex-preview-pane-refresh)
;;      (my/refresh-pdf-buffer)))
;;
;;  (defun my/refresh-pdf-buffer ()
;;    "Revert the PDF buffer to reload the updated PDF file."
;;    (let ((pdf-buffer (get-buffer "*latex-preview-pane*")))
;;      (when (and pdf-buffer
;;                 (with-current-buffer pdf-buffer
;;                   (string-match-p "\\.pdf$" buffer-file-name)))
;;        (with-current-buffer pdf-buffer
;;          (revert-buffer :ignore-auto :noconfirm)))))
;;
;;  ;; --- Custom Functions ---
;;
;;  (defun my/latex-project-root ()
;;    "Find the project root by locating 'main.tex'."
;;    (locate-dominating-file buffer-file-name "main.tex"))
;;
;;  (defun my/latex-main-tex-file ()
;;    "Get the full path to the main.tex."
;;    (expand-file-name "main.tex" (my/latex-project-root)))
;;
;;  (defun my/latex-aux-directory ()
;;    "Return the aux directory relative to the project root."
;;    (expand-file-name ".cache/latex" (my/latex-project-root)))
;;
;;  (defun my/latex-pdf-file ()
;;    "Return the PDF file to preview."
;;    (expand-file-name "main.pdf" (my/latex-project-root)))
;;
;;  (defun my/setup-latex-environment ()
;;    "Setup LaTeX environment for multifile documents."
;;    (latex-preview-pane-mode 1)
;;    ;; Force TeX-master to always be main.tex
;;    (setq-local TeX-master (my/latex-main-tex-file))
;;    (setq-local latex-preview-pane-pdf-file (my/latex-pdf-file))
;;    (add-hook 'after-save-hook #'my/latex-compile-main-and-refresh nil t)
;;    ;; Optionally trigger initial compile immediately
;;    ;; (my/latex-compile-main-and-refresh)
;;    ))
;;
;;;; --- Preview Pane Size Functions ---
;;
;;(defun my/latex-preview-bigger ()
;;  "Increase the size of the preview pane."
;;  (interactive)
;;  (setq latex-preview-pane-size (+ latex-preview-pane-size 20))
;;  (latex-preview-pane-refresh))
;;
;;(defun my/latex-preview-smaller ()
;;  "Decrease the size of the preview pane."
;;  (interactive)
;;  (setq latex-preview-pane-size (max 40 (- latex-preview-pane-size 20)))
;;  (latex-preview-pane-refresh))
;;
;;;; --- Keybindings ---
;;
;;(defun my/latex-setup-keybindings ()
;;  "Setup keybindings for LaTeX-mode preview controls."
;;  (define-key LaTeX-mode-map (kbd "C-c }") #'my/latex-preview-bigger)
;;  (define-key LaTeX-mode-map (kbd "C-c {") #'my/latex-preview-smaller))
;;
;;(add-hook 'LaTeX-mode-hook #'my/latex-setup-keybindings)

(provide 'latex-conf)
;;; latex-conf.el ends here


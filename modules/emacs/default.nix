{ lib, pkgs, config, home, ... }:
with lib;

let 
  cfg = config.editors.emacs;
in {
  options.editors.emacs = {
    enable = mkEnableOption "Install and setup emacs";
  };

  config = mkIf cfg.enable {

    programs.emacs = {
      enable = true;
      package = if pkgs.stdenv.isDarwin 
        then pkgs.emacs
        else pkgs.emacs;
      extraConfig = builtins.concatStringsSep "\n" [
        (builtins.readFile ./latex-conf.el)
        (builtins.readFile ./markdown-conf.el)
        (builtins.readFile ./maths-blocks.el)
        (builtins.readFile ./org-conf.el)
        (builtins.readFile ./config.el)
        (builtins.readFile ./theme-conf.el)        
        ''
      ; (setq agda2-program "${pkgs.agda}/bin/agda")

      (load-file
       (let ((coding-system-for-read 'utf-8))
        ; (shell-command-to-string "${pkgs.agda}/bin/agda-mode locate")))
        (shell-command-to-string "agda-mode locate")))
      (add-to-list 'auto-mode-alist '("\\.lagda\\.md\\'" . agda2-mode))

      (tool-bar-mode 0)
        ''
      ];

      extraPackages = epkgs: with epkgs;
        [ evil
          evil-visual-mark-mode
          company
          haskell-mode
          eglot
          direnv
          org
          org-bullets
          org-special-block-extras
          markdown-mode
          auctex
          mixed-pitch
          nix-mode
          ligature
          monokai-theme
          moody
          auto-dark
          material-theme
          exec-path-from-shell
          obsidian
        ];
    };

    home.file = {
      ".emacs.d/early-init.el".source = ./init-early.el;
      ".emacs.d/emacs.png".source = ./emacs.png;
    };

    xdg.configFile = {
      "dictionaries" = {
        recursive = true;
        source = ./dictionaries;
      };
    };

    home.packages = [
      pkgs.ispell
      pkgs.hunspell
    ];
  };
}



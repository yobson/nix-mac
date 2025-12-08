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
        then pkgs.emacs-unstable.overrideAttrs (old: {
        patches =
          (old.patches or []
            ++ [
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-28/fix-window-role.patch";
                sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
              })
              (pkgs.fetchpatch {
                url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/round-undecorated-frame.patch";
                sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
              })
            ]);
      })
      else pkgs.emacs;
      extraConfig = builtins.concatStringsSep "\n" [
        (builtins.readFile ./emacs/latex-conf.el)
        (builtins.readFile ./emacs/markdown-conf.el)
        (builtins.readFile ./emacs/maths-blocks.el)
        (builtins.readFile ./emacs/org-conf.el)
        (builtins.readFile ./emacs/config.el)
        (builtins.readFile ./emacs/theme-conf.el)        
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
        ];
    };

    home.file = {
      ".emacs.d/early-init.el".source = ./emacs/init-early.el;
      ".emacs.d/emacs.png".source = ./emacs/emacs.png;
    };

    xdg.configFile = {
      "dictionaries" = {
        recursive = true;
        source = ./emacs/dictionaries;
      };
    };

    home.packages = [
      pkgs.ispell
      pkgs.hunspell
    ];
  };
}



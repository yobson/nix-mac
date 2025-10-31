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
        (builtins.readFile ./latex-conf.el)
        (builtins.readFile ./markdown-conf.el)
        (builtins.readFile ./maths-blocks.el)
        (builtins.readFile ./org-conf.el)
        (builtins.readFile ./config.el)
        (builtins.readFile ./theme-conf.el)        
        ''
      (setq agda2-program "${pkgs.agda}/bin/agda")

      (load-file
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "${pkgs.agda}/bin/agda-mode locate")))

      (tool-bar-mode 0)
        ''
        #(builtins.readFile ./org-agda-mode.el)
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
          polymode
          poly-markdown
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
      ".emacs.d/early-init.el".source = ./init-early.el;
      ".emacs.d/emacs.png".source = ./emacs.png;
    };
  };
}



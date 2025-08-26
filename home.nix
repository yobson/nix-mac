{ config, pkgs, username, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = "/Users/${username}";

  home.packages = [
    (pkgs.writeShellScriptBin "nix-rebuild" ''
       sudo darwin-rebuild switch --flake /Users/${username}/.config/nix
    '')
    pkgs.audacity
    pkgs.iterm2
    pkgs.net-news-wire
    pkgs.skimpdf
    pkgs.pinentry_mac
    pkgs.unnaturalscrollwheels
    pkgs.ispell
    pkgs.hunspell
    (pkgs.agda.withPackages (p: [ p.standard-library ]))
  ];

  programs.bash = {
    enable = true;
    initExtra = ''
      source ~/.ghcup/env
      export PATH=$PATH:/Users/jameshobson/.local/bin
      export PLAN9=${pkgs.plan9port}/plan9
      export PATH=$PATH:${pkgs.plan9port}/plan9/bin
      export LANG=en_GB.UTF-8
    '';
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    pinentry.package = pkgs.pinentry_mac;
    enableScDaemon = false;
  };

  programs.git = {
    enable = true;
    ignores = [
      "*.vscode"
      "dist-newstyle"
    ];
    userEmail = "james@hobson.space";
    userName = "James Hobson";
    signing.signByDefault = true;
    signing.key = "D5E8 7B99 20A0 F392 857E  6212 27B6 62CE FCE9 BE00";
  };

  programs.powerline-go = {
    enable = true;
    settings = {
      jobs = "$(jobs -p | wc -l)";
    };
    pathAliases = {
      "\\~/gits" = "";
      "/Volumes/Projects" = "";
      "\\~/syncthing" = "";
    };
    modules = [
      "nix-shell"
      "cwd"
      "git"
      "jobs"
      "root"
    ];
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vimtex
      nvim-treesitter.withAllGrammars
      lualine-nvim
      tabline-nvim
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      agda-vim
      melange-nvim
      nerdtree
      vim-devicons
      nvim-lspconfig
      markdown-preview-nvim
      # cornelis
    ];
    extraLuaConfig = ''
      require("config")
    '';
    extraConfig = builtins.readFile ./dotfiles/vimrc;
    extraPackages = [ pkgs.cornelis ];
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs.overrideAttrs (old: {
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
    });
    extraConfig = builtins.concatStringsSep "\n" [
      (builtins.readFile ./emacs/latex-conf.el)
      (builtins.readFile ./emacs/markdown-conf.el)
      (builtins.readFile ./emacs/maths-blocks.el)
      (builtins.readFile ./emacs/org-conf.el)
      (builtins.readFile ./dotfiles/emacs)
      ''
      (setq agda2-program "${pkgs.agda}/bin/agda")
      
      (load-file
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "${pkgs.agda}/bin/agda-mode locate")))
      ''
      # (builtins.readFile ./emacs/org-agda-mode.el)
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
        nix-mode
        nano-theme
        timu-macos-theme
        ligature
      ];
  };

  xdg.configFile = {
    "nvim/lua/config" = {
      recursive = true;
      source = ./nvim-lua;
    };
    "emacs" = {
      recursive = true;
      source = ./emacs;
    };
    "dictionaries" = {
      recursive = true;
      source = ./dictionaries;
    };
    "scripts" = {
      recursive = true;
      source = ./dotfiles/scripts;
    };
  };

  home.file = {
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ghci".source = ./dotfiles/ghci;
    ".emacs".source = ./dotfiles/emacs;

    "Library/KeyBindings/DefaultKeyBindingNix.dict" = {
      source = ./DefaultKeyBinding.dict;
      onChange = ''
        rm -f ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
        cp ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBindingNix.dict ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
        chmod 764 ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
      '';
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  #  programs.vscode = {
  #    enable = true;
  #    package = pkgs.vscodium;
  #    profiles.default.extensions = with pkgs.vscode-extensions; [
  #      vscodevim.vim
  #      haskell.haskell
  #      mkhl.direnv
  #      banacorn.agda-mode
  #    ];
  #  };

  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
}

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
    package = pkgs.emacsMacport;
    extraConfig = builtins.concatStringsSep "\n" [
      # latex-conf.el		markdown-conf.el	maths-blocks.el		org-agda-mode.el	org-conf.el
      (builtins.readFile ./emacs/latex-conf.el)
      (builtins.readFile ./emacs/markdown-conf.el)
      (builtins.readFile ./emacs/maths-blocks.el)
      # (builtins.readFile ./emacs/org-agda-mode.el)
      (builtins.readFile ./emacs/org-conf.el)
      (builtins.readFile ./dotfiles/emacs)
    ];

    extraPackages = epkgs: with epkgs;
      [ evil
        evil-visual-mark-mode
        company
        haskell-mode
        eglot
        spacemacs-theme
        direnv
        org
        org-bullets
        org-special-block-extras
        markdown-mode
        auctex
        mixed-pitch
        polymode
	nix-mode
	inkpot-theme
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

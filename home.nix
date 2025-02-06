{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jameshobson";
  home.homeDirectory = /Users/jameshobson;

  home.packages = [
    (pkgs.writeShellScriptBin "haskell" ''
       nix-shell -p ghc cabal-install haskell-language-server
    '')
  ];

  programs.bash = {
    enable = true;
    initExtra = ''
      source ~/.ghcup/env
      export PATH=$PATH:/Users/jameshobson/.local/bin
    '';
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    pinentryPackage = pkgs.pinentry_mac;
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
      coq_nvim
      agda-vim
      { 
        plugin = nvim-lspconfig;
        type = "lua";
        config = ''
          require('lspconfig')['hls'].setup{
            filetypes = { 'haskell', 'lhaskell', 'cabal' },
          }
        '';
      }
    ];
    extraLuaConfig = ''
      require("config")
    '';
    extraConfig = ''
      filetype plugin on
      syntax on
      set number
      set expandtab
      set tabstop=2
      set shiftwidth=2
      set splitright
    '';
  };

  xdg.configFile."nvim/lua/config" = {
      recursive = true;
      source = ./nvim-lua;
    };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      haskell.haskell
      mkhl.direnv
      banacorn.agda-mode
    ];
  };

  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
}

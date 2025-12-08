{ lib, pkgs, config, home, ... }:
with lib;

let 
  cfg = config.editors.vim;
in {
  options.editors.vim = {
    enable = mkEnableOption "Install and setup neovim";
  };

  config = mkIf cfg.enable {
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
      extraConfig = builtins.readFile ./vim/vimrc;
      extraPackages = [ pkgs.cornelis ];
    };


    xdg.configFile."nvim/lua/config" = {
      recursive = true;
      source = ./vim/config;
    };
  };
}


{ config, lib, pkgs, username, ... }:

{
  imports = [ ./modules/emacs/emacs.nix
              ./modules/vim/vim.nix
              ./modules/keyboard.nix
              ./modules/xmonad.nix
            ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = if pkgs.stdenv.isDarwin 
    then "/Users/${username}"
    else "/home/${username}";

  home.packages = [
    pkgs.audacity
    pkgs.ispell
    pkgs.hunspell
    pkgs.musescore
    (pkgs.callPackage ./pkgs/gforth.nix {})
    (pkgs.agda.withPackages (p: [ p.standard-library ]))
  ] ++ lib.optionals (pkgs.stdenv.isDarwin) [
    (pkgs.writeShellScriptBin "nix-rebuild" ''
       sudo darwin-rebuild switch --flake /Users/${username}/.config/nix
       '')
    pkgs.iterm2
    pkgs.net-news-wire
    pkgs.skimpdf
    pkgs.unnaturalscrollwheels
    pkgs.iina
    pkgs.vlc-bin
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
      pkgs.vlc
      pkgs.racket
      pkgs.kicad
      pkgs.firefox
      pkgs.transmission-remote-gtk
      pkgs.curl
      pkgs.gcc
      pkgs.gmp
      pkgs.gnumake
      pkgs.ncurses
      pkgs.pkg-config
  ];

  programs.bash = {
    enable = true;
    initExtra = ''
      source ~/.ghcup/env
      export PATH=$PATH:$HOME/.local/bin
      export PLAN9=${pkgs.plan9port}/plan9
      export PATH=$PATH:${pkgs.plan9port}/plan9/bin
      export LANG=en_GB.UTF-8
      export EDITOR=vim
    '';
  };

  programs.gpg = {
    enable = true;
    settings = {
      keyserver = "hkps://keyserver.ubuntu.com";
    };
  };
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    pinentry.package = if pkgs.stdenv.isDarwin 
      then pkgs.pinentry_mac
      else pkgs.pinentry-qt;
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

  editors.emacs.enable = true;
  editors.vim.enable = true;

  x11.xmonad.enable = pkgs.stdenv.isLinux;

  xdg.configFile = {
    "dictionaries" = {
      recursive = true;
      source = ./dotfiles/dictionaries;
    };
    "scripts/open-term.sh" = {
      source = pkgs.replaceVars ./dotfiles/scripts/open-term.sh {
        iTerm2 = if pkgs.stdenv.isDarwin
          then "${pkgs.iterm2}/Applications/iTerm2.app"
          else "";
      };
      executable = true;
    };
    "scripts/switch.sh" = {
      source = ./dotfiles/scripts/switch.sh;
      executable = true;
    };
  };

  home.file = {
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ghci".source = ./dotfiles/ghci;
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

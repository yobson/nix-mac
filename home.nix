{ config, lib, pkgs, username, ... }:

{
  imports = [ ./modules/vim
              ./modules/keyboard
            ];

  home.username = username;
  home.homeDirectory = if pkgs.stdenv.isDarwin 
    then "/Users/${username}"
    else "/home/${username}";

  home.packages = with pkgs; [
    (callPackage ./pkgs/gforth.nix {})
    plan9port
  ];

  targets.darwin.copyApps.enable = false;
  targets.darwin.linkApps.enable = pkgs.stdenv.isDarwin;

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
    settings.user = {
      userEmail = "james@hobson.space";
      userName = "James Hobson";
    };
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
      "ssh"
      "nix-shell"
      "cwd"
      "git"
      "jobs"
      "root"
    ];
  };

  editors.vim.enable = true;


  home.file = {
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ghci".source = ./dotfiles/ghci;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };


  home.stateVersion = "25.11";

  programs.home-manager.enable = true;
}

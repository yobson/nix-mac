{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jameshobson";
  home.homeDirectory = /Users/jameshobson;

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
    signing.key = "D5E8";
  };


  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
}

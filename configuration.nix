{ pkgs, config, homedir, user, ... }: 
{
  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      ref = "master";
      rev = "b4ffd36bc464accccc8868d7ec498eeb21c6d272";
    }))
  ];

  imports = [./modules/windows.nix];
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    let tex = (pkgs.texlive.combine {
      inherit (pkgs.texlive) scheme-basic
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of
      fontspec bbold bboldx bbold-type1 esint
      metafont collection-fontsextra collection-fontsrecommended
      ec cm-super parskip;
    });
    in [ mkalias
      gnupg
      quilt
      tex
      plan9port
      ffmpeg_6-full
    ];

  fonts.packages = with pkgs; [
    fira-code
    font-awesome
    powerline-fonts
    powerline-symbols
    nerd-fonts.symbols-only
  ];

  homebrew = {
    enable = true;
    brews = [
      "findutils"
      "gnu-indent"
      "gnu-sed"
      "gnutls"
      "grep"
      "gnu-tar"
      "gawk"
    ];
    #masApps = {
    #  "UTM" = 1538878817;
    #  "reMarkable" = 1276493162;
    #  "(beat)" = 1549538329;
    #  "Zeroconf Browser" = 1355001318;
    #};
    #onActivation.autoUpdate = true;
    #onActivation.upgrade = true;
  };

  system.defaults = {
    dock = {
      autohide = true;
      tilesize = 42;
      show-recents = false;
      show-process-indicators = true;
      persistent-apps = [
        "/System/Cryptexes/App/System/Applications/Safari.app"
        "/System/Applications/Apps.app"
        "/Applications/reMarkable.app"
        "/Applications/Notion.app"
        "${pkgs.iterm2}/Applications/iTerm2"
        "/System/Applications/Mail.app"
        "/System/Applications/Messages.app"
        "/System/Applications/Calendar.app"
        "/System/Applications/Notes.app"
        "/System/Applications/TV.app"
        "/System/Applications/Home.app"
        #gpg keychain
        "/System/Applications/Music.app"
        "/System/Applications/App Store.app"
        "/System/Applications/Passwords.app"
        "/System/Applications/iPhone Mirroring.app"
        "${pkgs.emacsMacport}/Applications/Emacs.app"
        "${pkgs.net-news-wire}/Applications/NetNewsWire.app"
        "/System/Applications/System Settings.app"
      ];
    };
    loginwindow.GuestEnabled = false;
    NSGlobalDomain.AppleICUForce24HourTime = true;
    NSGlobalDomain._HIHideMenuBar = false;
    NSGlobalDomain.NSWindowShouldDragOnGesture = true;
  };

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;
  security.pam.services.sudo_local.touchIdAuth = true;

  nix.settings.experimental-features = "nix-command flakes";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  programs.bash.enable = true;
  environment.shells = with pkgs; [ bashInteractive ];



  system.primaryUser = user;
  users.users.${user} = {
    name = user;
    home = homedir;
  };

}


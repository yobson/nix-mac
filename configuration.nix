{ pkgs, config, ... }: 
{
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs;
    [ vim
      mkalias
      unnaturalscrollwheels
      iterm2
      gnupg
      pinentry_mac
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
    masApps = {
      "UTM" = 1538878817;
      "reMarkable" = 1276493162;
    };
    onActivation.autoUpdate = true;
    onActivation.upgrade = true;
  };

  system.defaults = {
    dock.persistent-apps = [
      "/System/Cryptexes/App/System/Applications/Safari.app"
      "/System/Applications/Launchpad.app"
      "/Applications/reMarkable.app"
      "/System/Applications/Mail.app"
      "/System/Applications/Messages.app"
      "/System/Applications/Calendar.app"
      "/System/Applications/Notes.app"
      "/System/Applications/TV.app"
      # notion
      "/System/Applications/Home.app"
      "${pkgs.iterm2}/Applications/iTerm2.app"
      #gpg keychain
      "/System/Applications/Music.app"
      "/System/Applications/App Store.app"
      "/System/Applications/Passwords.app"
      "/System/Applications/iPhone Mirroring.app"
      "/System/Applications/System Settings.app"
    ];
    loginwindow.GuestEnabled = false;
    NSGlobalDomain.AppleICUForce24HourTime = true;
  };

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;


  system.activationScripts.applications.text = let
    env = pkgs.buildEnv {
      name = "system-applications";
      paths = config.environment.systemPackages;
      pathsToLink = "/Applications";
    };
  in
    pkgs.lib.mkForce ''
    # Set up applications.
    echo "setting up /Applications..." >&2
    rm -rf /Applications/Nix\ Apps
    mkdir -p /Applications/Nix\ Apps
    find ${env}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
    while read -r src; do
      app_name=$(basename "$src")
      echo "copying $src" >&2
      ${pkgs.mkalias}/bin/mkalias "$src" "/Applications/Nix Apps/$app_name"
    done
  '';

  nix.settings.experimental-features = "nix-command flakes";

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  nixpkgs.hostPlatform = "aarch64-darwin";
}


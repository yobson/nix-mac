{ config, lib, pkgs, ... }:

{
  services = {
    gnome.gnome-keyring.enable = true;
    upower.enable = true;

    dbus = {
      enable = true;
    };

    libinput = {
      enable = true;
    };

    displayManager = { 
      # defaultSession = "none+xmonad";
      ly.enable = false;
      gdm = {
        enable = true;
      };
    };

    desktopManager.gnome.enable = true;


    xserver = {
      enable = false;

      windowManager.xmonad = {
        enable = false;
        enableContribAndExtras = true;
      };
      desktopManager.enlightenment.enable = false;

      xkb.options = "caps:escape";
    };
  };
  environment.enlightenment.excludePackages = with pkgs.enlightenment; [
    econnman
  ];

  environment.systemPackages = with pkgs.gnomeExtensions; [
    dash-to-dock
    paperwm
  ];

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  systemd.services.upower.enable = true;
}

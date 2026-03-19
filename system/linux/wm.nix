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
      ly.enable = true;
      gdm = {
        enable = false;
        wayland = false;
      };
    };


    xserver = {
      enable = true;

      windowManager.xmonad = {
        enable = false;
        enableContribAndExtras = true;
      };
      desktopManager.enlightenment.enable = true;

      xkb.options = "caps:escape";
    };
  };
  environment.enlightenment.excludePackages = with pkgs.enlightenment; [
    econnman
  ];

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  systemd.services.upower.enable = true;
}

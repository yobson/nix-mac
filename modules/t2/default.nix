{pkgs, config, ...}:

{
  systemd.services.suspend-fix-t2 = {
    enable = true;
    description = "Disable and Re-Enable Apple BCE Module (and Wi-Fi)";
    before = ["sleep.target"];
    wantedBy = ["sleep.target"];
    unitConfig = {
      StopWhenUnneeded = true;
    };

    serviceConfig = {
      User = "root";
      Type = "oneshot";
      RemainAfterExit = "yes";
      ExecStart = [ 
        "${pkgs.kmod}/bin/rmmod -f apple-bce"
        "${pkgs.kmod}/bin/modprobe -r hid_appletb_kbd"
        "${pkgs.kmod}/bin/modprobe -r hid_appletb_bl"
      ];

      ExecStop = [
        "${pkgs.coreutils-full}/bin/sleep 4"
        "${pkgs.kmod}/bin/modprobe apple-bce"
        "${pkgs.coreutils-full}/bin/sleep 4"
        "${pkgs.kmod}/bin/modprobe hid_appletb_bl"
        "${pkgs.coreutils-full}/bin/sleep 2"
        "${pkgs.kmod}/bin/modprobe hid_appletb_kbd"
      ];
    };
  };

  hardware.apple.touchBar = { 
    enable = true;
    settings = { 
      AdaptiveBrightness = true;
      MediaLayerDefault = true;
    };
  };

}

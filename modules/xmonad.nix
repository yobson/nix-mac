{pkgs, config, lib, home, ...}:
with lib;

let
  cfg = config.x11.xmonad;
  libDir = ./xmonad/src;  # directory containing files
  files = builtins.readDir libDir;

  # readDir gives { file1 = "regular"; file2 = "regular"; ... }
  # so turn them into an attrset mapping names to paths
  libFiles =
    builtins.mapAttrs (name: _type: "${libDir}/${name}") files;
in {
  options.x11.xmonad = {
    enable = mkEnableOption "Enable XMonad Config";
    wallpaper = mkOption {
      type = types.path;
      default = "${pkgs.gnome-backgrounds}/share/backgrounds/gnome/map-d.svg";
    };
  };
  config = {
    xsession = {
      enable = cfg.enable;

      # initExtra = extra + polybarOpts;

      windowManager.xmonad = {
        enable = cfg.enable;
        enableContribAndExtras = true;
        extraPackages = hp: [
          hp.dbus
          hp.monad-logger
        ];
        config = pkgs.replaceVars ./xmonad/xmonad.hs {
          wezterm   = "${pkgs.wezterm}/bin/wezterm";
          rofi      = "${pkgs.rofi}/bin/rofi";
          feh       = "${pkgs.feh}/bin/feh";
          wallpaper = cfg.wallpaper;
        };
        libFiles = libFiles;
      };
    };

    programs.rofi = {
      enable   = cfg.enable;
      terminal = "${pkgs.wezterm}/bin/wezterm";
    };

    home.packages = optionals cfg.enable [
      pkgs.wezterm
      pkgs.feh
      pkgs.gnome-backgrounds
    ];
  };
}

{pkgs, config, lib, home, ...}:
with lib;

let
  cfg = config.x11.xmonad;

in {
  options.x11.xmonad = {
    enable = mkEnableOption "Enable XMonad Config";
    wallpaper = mkOption {
      type = types.path;
      default = "${pkgs.gnome-backgrounds}/share/backgrounds/gnome/map-d.svg";
    };
    rofi-theme = mkOption {
      type = types.path;
      default = ./xmonad/rofi-theme.rasi;
    };
    polybar-config = mkOption {
      type = types.path;
      default = pkgs.replaceVars ./xmonad/polybar.ini {
        xmonad-log = "${pkgs.xmonad-log}/bin/xmonad-log";
      };
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
          hp.monad-logger
          hp.stm
          hp.typed-process
          hp.bytestring
        ];
        config = pkgs.replaceVars ./xmonad/app/Main.hs {
          wezterm    = "${pkgs.wezterm}/bin/wezterm";
          rofi       = "${pkgs.rofi}/bin/rofi";
          feh        = "${pkgs.feh}/bin/feh";
          wallpaper  = cfg.wallpaper;
        };
        libFiles = {
          "Rofi.hs" = pkgs.replaceVars ./xmonad/app/Rofi.hs {
            rofi      = "${pkgs.rofi}/bin/rofi";
          };
          "QS.hs" = pkgs.replaceVars ./xmonad/app/QS.hs {
            quickshell = "${pkgs.quickshell}/bin/quickshell";
          };
          "Monitor.hs" = ./xmonad/app/Monitor.hs;
        };
      };
    };

 #    xdg.configFile."quickshell" = {
 #      recursive = true;
 #      source = ./xmonad/quickshell;
 #    };

    programs.rofi = {
      enable   = cfg.enable;
      terminal = "${pkgs.wezterm}/bin/wezterm";
      theme    = cfg.rofi-theme;
    };

    services.dunst = {
      enable = cfg.enable;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
        size = "16x16";
      };
      settings = {
        global = {
          monitor = 0;
          geometry = "600x50-50+65";
          shrink = "yes";
          transparency = 10;
          padding = 16;
          horizontal_padding = 16;
          font = "JetBrainsMono Nerd Font 10";
          line_height = 4;
          format = ''<b>%s</b>\n%b'';
        };
      };
    };

    services.polybar = {
      enable = false; # cfg.enable;
      package = pkgs.polybar;
      config = cfg.polybar-config;
      script = ''
      polybar top &
      '';
    };

    home.packages = optionals cfg.enable [
      pkgs.wezterm
      pkgs.feh
      pkgs.gnome-backgrounds
      pkgs.xmonad-log
      pkgs.quickshell
    ];
  };
}

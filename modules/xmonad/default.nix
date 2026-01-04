{pkgs, config, lib, home, ...}:
with lib;

let
  cfg = config.x11.xmonad;
  ninep = pkgs.haskellPackages.callPackage ../../pkgs/ninepmonad.nix {};

in {
  options.x11.xmonad = {
    enable = mkEnableOption "Enable XMonad Config";
    wallpaper = mkOption {
      type = types.path;
      default = "${pkgs.gnome-backgrounds}/share/backgrounds/gnome/map-d.svg";
    };
    rofi-theme = mkOption {
      type = types.path;
      default = ./rofi-theme.rasi;
    };
    polybar-config = mkOption {
      type = types.path;
      default = pkgs.replaceVars ./polybar.ini {
        xmonad-log = "${pkgs.xmonad-log}/bin/xmonad-log";
      };
    };
    terminal = mkOption {
      type = types.str;
      default = "${pkgs.wezterm}/bin/wezterm";
    };
  };
  config = {
    xsession = {
      enable = cfg.enable;

      windowManager.xmonad = {
        enable = cfg.enable;
        enableContribAndExtras = true;
        extraPackages = hp: [
          hp.monad-logger
          hp.stm
          hp.typed-process
          hp.bytestring
          ninep
        ];
        config = pkgs.replaceVars ./app/Main.hs {
          terminal   = cfg.terminal;
          rofi       = "${pkgs.rofi}/bin/rofi";
          feh        = "${pkgs.feh}/bin/feh";
          wallpaper  = cfg.wallpaper;
        };
        libFiles = {
          "Rofi.hs" = pkgs.replaceVars ./app/Rofi.hs {
            rofi      = "${pkgs.rofi}/bin/rofi";
          };
          "QS.hs" = pkgs.replaceVars ./app/QS.hs {
            quickshell = "${pkgs.quickshell}/bin/quickshell";
          };
          "Monitor.hs" = ./app/Monitor.hs;
          "FileSystem.hs" = ./app/FileSystem.hs;
        };
        buildScript = ./build;
      };
    };


    #    xdg.configFile."quickshell" = {
    #      recursive = true;
    #      source = ./quickshell;
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
      pkgs.feh
      pkgs.gnome-backgrounds
      pkgs.xmonad-log
      pkgs.quickshell
    ] ++ optionals (cfg.enable && cfg.terminal == "${pkgs.wezterm}/bin/wezterm") [ pkgs.wezterm ];
  };
}

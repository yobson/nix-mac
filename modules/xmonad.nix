{pkgs, config, lib, home, ...}:
with lib;

let
  cfg = config.x11.xmonad;
  ninep = pkgs.haskellPackages.callPackage ../pkgs/ninepmonad.nix {};

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
    terminal = mkOption {
      type = types.str;
      default = "${pkgs.wezterm}/bin/wezterm";
    };
  };
  config = {
    xsession = {
      enable = cfg.enable;
    };

    xdg.configFile = {
      "xmonad-src/flake.nix".source  = ./xmonad/flake.nix;
      "xmonad-src/flake.lock".source = ../flake.lock;
      "xmonad-src/xmonad.cabal".source = ./xmonad/xmonad.cabal;
      "xmonad-src/deps" = { source = ../pkgs; recursive = true; };
      "xmonad-src/app/Main.hs".source = pkgs.replaceVars ./xmonad/app/Main.hs {
        terminal   = cfg.terminal;
        rofi       = "${pkgs.rofi}/bin/rofi";
        feh        = "${pkgs.feh}/bin/feh";
        wallpaper  = cfg.wallpaper;
      };
      "xmonad-src/app/Rofi.hs".source = pkgs.replaceVars ./xmonad/app/Rofi.hs {
        rofi      = "${pkgs.rofi}/bin/rofi";
      };
      "xmonad-src/app/QS.hs".source = pkgs.replaceVars ./xmonad/app/QS.hs {
        quickshell = "${pkgs.quickshell}/bin/quickshell";
      };
      "xmonad-src/app/Monitor.hs".source = ./xmonad/app/Monitor.hs;
      "xmonad-src/app/FileSystem.hs".source = ./xmonad/app/FileSystem.hs;
    };

    home.activation."XMonad" = lib.hm.dag.entryAfter ["onFilesChange"] ''
      rm -fr ~/.config/xmonad
      mkdir -p ~/.config/xmonad
        cp -rL ${config.home.homeDirectory}/.config/xmonad-src/* ${config.home.homeDirectory}/.config/xmonad
      '';

      #config = pkgs.replaceVars ./xmonad/app/Main.hs {
      #  terminal   = cfg.terminal;
      #  rofi       = "${pkgs.rofi}/bin/rofi";
      #  feh        = "${pkgs.feh}/bin/feh";
      #  wallpaper  = cfg.wallpaper;
      #};
      #libFiles = {
      #  "Rofi.hs" = pkgs.replaceVars ./xmonad/app/Rofi.hs {
      #    rofi      = "${pkgs.rofi}/bin/rofi";
      #  };
      #  "QS.hs" = pkgs.replaceVars ./xmonad/app/QS.hs {
      #    quickshell = "${pkgs.quickshell}/bin/quickshell";
      #  };
      #  "Monitor.hs" = ./xmonad/app/Monitor.hs;
      #  "FileSystem.hs" = ./xmonad/app/FileSystem.hs;
      #};

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
      pkgs.feh
      pkgs.gnome-backgrounds
      pkgs.xmonad-log
      pkgs.quickshell
    ] ++ optionals (cfg.enable && cfg.terminal == "${pkgs.wezterm}/bin/wezterm") [ pkgs.wezterm ];
  };
}

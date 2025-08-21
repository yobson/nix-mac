{ pkgs, lib, config, ... }: {
  services.skhd = {
    enable = true;
    skhdConfig = builtins.readFile ./dotfiles/skhdrc.conf;
  };

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
    config = {
      mouse_follows_focus          = "off";
      focus_follows_mouse          = "autoraise";
      window_origin_display        = "default";
      window_placement             = "second_child";
      window_topmost               = "on";
      window_shadow                = "on";
      window_opacity               = "on";
      window_opacity_duration      = 0.0;
      active_window_opacity        = 1.0;
      normal_window_opacity        = 0.90;
      window_border                = "off";
      window_border_width          = 6;
      active_window_border_color   = "0xff775759";
      normal_window_border_color   = "0xff555555";
      insert_feedback_color        = "0xffd75f5f";
      split_ratio                  = 0.5;
      auto_balance                 = "on";
      mouse_modifier               = "fn";
      mouse_action1                = "move";
      mouse_action2                = "resize";
      mouse_drop_action            = "swap";
      layout                       = "bsp";
      top_padding                  = 12;
      bottom_padding               = 12;
      left_padding                 = 12;
      right_padding                = 12;
      window_gap                   = 06;
    };
    extraConfig = ''
      yabai -m rule --add app="System Settings" manage=off
      yabai -m rule --add app="Finder" manage=off
      yabai -m rule --add app="Logi Options" manage=off
      yabai -m rule --add app="balenaEtcher" manage=off
      yabai -m rule --add app="Installer" manage=off
      yabai -m rule --add app="JetBrains Toolbox" manage=off
      yabai -m rule --add title="Outlook Preferences" manage=off
    '';
  };

  services.sketchybar = {
    enable = true;
    config = builtins.concatStringsSep "\n" [
               (builtins.readFile ./sketchy/colors.sh)
               (builtins.readFile ./sketchy/icon_map.sh)
               (builtins.readFile ./sketchy/add_separator.sh)
               (builtins.readFile ./sketchy/sketchybarrc)
               (builtins.readFile ./sketchy/sketchy-items/logo.sh)
               (builtins.readFile ./sketchy/sketchy-items/spaces.sh)
               (builtins.readFile ./sketchy/sketchy-items/frontapp.sh)
               (builtins.readFile ./sketchy/sketchy-items/menus.sh)
               (builtins.readFile ./sketchy/sketchy-items/calendar.sh)
               (builtins.readFile ./sketchy/sketchy-items/mic.sh)
               (builtins.readFile ./sketchy/sketchy-items/volume.sh)
               (builtins.readFile ./sketchy/sketchy-items/battery.sh)
               (builtins.readFile ./sketchy/sketchy-items/wifi.sh)
               (builtins.readFile ./sketchy/sketchy-items/display.sh)
               (builtins.readFile ./sketchy/sketchy-items/more-menu.sh)
               (builtins.readFile ./sketchy/sketchy-items/packages.sh)
               (builtins.readFile ./sketchy/sketchy-items/controls.sh)
               (builtins.readFile ./sketchy/sketchy-items/music.sh)
               (builtins.readFile ./sketchy/sketchy-items/cpu.sh)
               (builtins.readFile ./sketchy/sketchyset.sh)
      ];
  };
  environment.systemPackages =
      lib.optionals config.services.sketchybar.enable [
      # pkgs.menubar-cli
        pkgs.imagemagick
      #  pkgs.macmon
      ];

    homebrew.brews = lib.optionals config.services.sketchybar.enable [
      "media-control"
    ];

    fonts.packages = lib.optionals config.services.sketchybar.enable [
      pkgs.sketchybar-app-font
    ];

  services.aerospace = {
    enable = false;
    settings = {
      after-startup-command = [ "layout tiles" ];
      gaps = {
        outer.left = 12;
        outer.bottom = 12;
        outer.top = 12;
        outer.right = 12;
        inner.horizontal = 8;
        inner.vertical = 8;
      };
    mode.main.binding = {
      alt-shift-q = "close";
      alt-enter = "exec-and-forget open -a ${pkgs.iterm2}/Applications/iTerm2.app $HOME";
      alt-f = "macos-native-fullscreen";
      alt-left = "focus left";
      alt-down = "focus down";
      alt-up = "focus up";
      alt-right = "focus right";

      alt-shift-left = "move left";
      alt-shift-down = "move down";
      alt-shift-up = "move up";
      alt-shift-right = "move right";

      alt-1 = "workspace 1";
      alt-2 = "workspace 2";
      alt-3 = "workspace 3";
      alt-4 = "workspace 4";
      alt-5 = "workspace 5";
      alt-6 = "workspace 6";
      alt-7 = "workspace 7";
      alt-8 = "workspace 8";
      alt-9 = "workspace 9";

      alt-shift-1 = "move-node-to-workspace 1";
      alt-shift-2 = "move-node-to-workspace 2";
      alt-shift-3 = "move-node-to-workspace 3";
      alt-shift-4 = "move-node-to-workspace 4";
      alt-shift-5 = "move-node-to-workspace 5";
      alt-shift-6 = "move-node-to-workspace 6";
      alt-shift-7 = "move-node-to-workspace 7";
      alt-shift-8 = "move-node-to-workspace 8";
      alt-shift-9 = "move-node-to-workspace 9";
    };
    };
  };
}

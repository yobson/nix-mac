{ config, pkgs, username, ... }:

{
  imports = [./modules/macCompose.nix ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = "/Users/${username}";

  home.packages = [
    (pkgs.writeShellScriptBin "nix-rebuild" ''
       sudo darwin-rebuild switch --flake /Users/${username}/.config/nix
       '')
    pkgs.audacity
    pkgs.iterm2
    pkgs.net-news-wire
    pkgs.skimpdf
    pkgs.pinentry_mac
    pkgs.unnaturalscrollwheels
    pkgs.ispell
    pkgs.hunspell
    (pkgs.agda.withPackages (p: [ p.standard-library ]))
  ];

  programs.bash = {
    enable = true;
    initExtra = ''
      source ~/.ghcup/env
      export PATH=$PATH:/Users/jameshobson/.local/bin
      export PLAN9=${pkgs.plan9port}/plan9
      export PATH=$PATH:${pkgs.plan9port}/plan9/bin
      export LANG=en_GB.UTF-8
    '';
  };

  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    pinentry.package = pkgs.pinentry_mac;
    enableScDaemon = false;
  };

  programs.git = {
    enable = true;
    ignores = [
      "*.vscode"
      "dist-newstyle"
    ];
    userEmail = "james@hobson.space";
    userName = "James Hobson";
    signing.signByDefault = true;
    signing.key = "D5E8 7B99 20A0 F392 857E  6212 27B6 62CE FCE9 BE00";
  };

  programs.powerline-go = {
    enable = true;
    settings = {
      jobs = "$(jobs -p | wc -l)";
    };
    pathAliases = {
      "\\~/gits" = "";
      "/Volumes/Projects" = "";
      "\\~/syncthing" = "";
    };
    modules = [
      "nix-shell"
      "cwd"
      "git"
      "jobs"
      "root"
    ];
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vimtex
      nvim-treesitter.withAllGrammars
      lualine-nvim
      tabline-nvim
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      agda-vim
      melange-nvim
      nerdtree
      vim-devicons
      nvim-lspconfig
      markdown-preview-nvim
      # cornelis
    ];
    extraLuaConfig = ''
      require("config")
    '';
    extraConfig = builtins.readFile ./dotfiles/vimrc;
    extraPackages = [ pkgs.cornelis ];
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable.overrideAttrs (old: {
      patches =
        (old.patches or []
          ++ [
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-30/round-undecorated-frame.patch";
              sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            })
          ]);
    });
    extraConfig = builtins.concatStringsSep "\n" [
      (builtins.readFile ./emacs/latex-conf.el)
      (builtins.readFile ./emacs/markdown-conf.el)
      (builtins.readFile ./emacs/maths-blocks.el)
      (builtins.readFile ./emacs/org-conf.el)
      (builtins.readFile ./emacs/config.el)
      ''
      (setq agda2-program "${pkgs.agda}/bin/agda")

      (load-file
       (let ((coding-system-for-read 'utf-8))
         (shell-command-to-string "${pkgs.agda}/bin/agda-mode locate")))

      (add-hook 'agda2-mode #'evil-mode)
      ''
      # (builtins.readFile ./emacs/org-agda-mode.el)
    ];

    extraPackages = epkgs: with epkgs;
      [ evil
        evil-visual-mark-mode
        company
        haskell-mode
        eglot
        direnv
        org
        org-bullets
        org-special-block-extras
        markdown-mode
        auctex
        mixed-pitch
        polymode
        nix-mode
        ligature
        monokai-theme
      ];
  };

  xdg.configFile = {
    "nvim/lua/config" = {
      recursive = true;
      source = ./nvim-lua;
    };
    "dictionaries" = {
      recursive = true;
      source = ./dictionaries;
    };
    "scripts/open-term.sh" = {
      source = pkgs.replaceVars ./dotfiles/scripts/open-term.sh {
        iTerm2 = "${pkgs.iterm2}/Applications/iTerm2.app";
      };
      executable = true;
    };
  };

  home.file = {
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ghci".source = ./dotfiles/ghci;

    #"Library/KeyBindings/DefaultKeyBindingNix.dict" = {
    #  source = ./DefaultKeyBinding.dict;
    #  onChange = ''
    #    rm -f ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
    #    cp ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBindingNix.dict ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
    #    chmod 764 ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
    #  '';
    #};
  };

  macCompose = {
    enable = true;
    mapping = {
      ba = "𝕒";
      bb = "𝕓";
      bc = "𝕔";
      bd = "𝕕";
      be = "𝕖";
      bf = "𝕗";
      bg = "𝕘";
      bh = "𝕙";
      bi = "𝕚";
      bj = "𝕛";
      bk = "𝕜";
      bl = "𝕝";
      bm = "𝕞";
      bn = "𝕟";
      bo = "𝕠";
      bp = "𝕡";
      bq = "𝕢";
      br = "𝕣";
      bs = "𝕤";
      bt = "𝕥";
      bu = "𝕦";
      bv = "𝕧";
      bw = "𝕨";
      bx = "𝕩";
      by = "𝕪";
      bz = "𝕫";
      bA = "𝔸";
      bB = "𝔹";
      bC = "ℂ";
      bD = "𝔻";
      bE = "𝔼";
      bF = "𝔽";
      bG = "𝔾";
      bH = "ℍ";
      bI = "𝕀";
      bJ = "𝕁";
      bK = "𝕂";
      bL = "𝕃";
      bM = "𝕄";
      bN = "ℕ";
      bO = "𝕆";
      bP = "ℙ";
      bQ = "ℚ";
      bR = "ℝ";
      bS = "𝕊";
      bT = "𝕋";
      bU = "𝕌";
      bV = "𝕍";
      bW = "𝕎";
      bX = "𝕏";
      bY = "𝕐";
      bZ = "ℤ";
      b0 = "𝟘";
      b1 = "𝟙";
      b2 = "𝟚";
      b3 = "𝟛";
      b4 = "𝟜";
      b5 = "𝟝";
      b6 = "𝟞";
      b7 = "𝟟";
      b8 = "𝟠";
      b9 = "𝟡";

      mca = "𝓪";
      mcb = "𝓫";
      mcc = "𝓬";
      mcd = "𝓭";
      mce = "𝓮";
      mcf = "𝓯";
      mcg = "𝓰";
      mch = "𝓱";
      mci = "𝓲";
      mcj = "𝓳";
      mck = "𝓴";
      mcl = "𝓵";
      mcm = "𝓶";
      mcn = "𝓷";
      mco = "𝓸";
      mcp = "𝓹";
      mcq = "𝓺";
      mcr = "𝓻";
      mcs = "𝓼";
      mct = "𝓽";
      mcu = "𝓾";
      mcv = "𝓿";
      mcw = "𝔀";
      mcx = "𝔁";
      mcy = "𝔂";
      mcz = "𝔃";
      mcA = "𝓐";
      mcB = "𝓑";
      mcC = "𝓒";
      mcD = "𝓓";
      mcE = "𝓔";
      mcF = "𝓕";
      mcG = "𝓖";
      mcH = "𝓗";
      mcI = "𝓘";
      mcJ = "𝓙";
      mcK = "𝓚";
      mcL = "𝓛";
      mcM = "𝓜";
      mcN = "𝓝";
      mcO = "𝓞";
      mcP = "𝓟";
      mcQ = "𝓠";
      mcR = "𝓡";
      mcS = "𝓢";
      mcT = "𝓣";
      mcU = "𝓤";
      mcV = "𝓥";
      mcW = "𝓦";
      mcX = "𝓧";
      mcY = "𝓨";
      mcZ = "𝓩";

      lt = "⊤";
      lf = "⊥";

      and = "∧";
      circ = "◯";
      Circ = "●";
      div = "÷";
      equiv = "≡";
      exists = "∃";
      forall = "∀";
      mapsto = "↦";
      neg = "¬";
      or = "∨";
      pi = "π";
      Pi = "Π";
      prod = "∏";
      qed = "∎";
      star = "★";
      sum = "∑";
      times = "×";
      to = "→";
      vdash = "⊢";
      vDash = "⊨";
      vee = "∨";
      wedge = "∧";

      "[[" = "⟦";
      "]]" = "⟧";
      ">>" = "⟩";
      "<<" = "⟨";
      "<=" = "≤";
      ">=" = "≥";
      ":=" = "≔";
      "|->" = "↦";
      "=" = "＝";
      "~-" = "≃";
      "~=" = "≅";
      "~~" = "≈";
      "~>" = "⇝";
      "|-" = "⊢";
      "|=" = "⊨";
      "."  = "∙";
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  #  programs.vscode = {
  #    enable = true;
  #    package = pkgs.vscodium;
  #    profiles.default.extensions = with pkgs.vscode-extensions; [
  #      vscodevim.vim
  #      haskell.haskell
  #      mkhl.direnv
  #      banacorn.agda-mode
  #    ];
  #  };

  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
}

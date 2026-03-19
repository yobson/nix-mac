{
  description = "James's Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # mac-app-util.url = "github:hraban/mac-app-util";
    mac-app-util.url = "github:mcflis/mac-app-util/fix/missing-icons";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-homebrew, home-manager, mac-app-util }:
    let homeConf = user: {
      home-manager.extraSpecialArgs = {
        username = user;
      };
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.${user} = {
        imports = [ 
          ./home.nix
          ./darwin.nix
          ./desktop.nix
          { editors.emacs.obsidianDir = "~/Obsidian/Notes"; }
        ];
      };
      home-manager.backupFileExtension = "hm-backup";
      home-manager.sharedModules = [
        mac-app-util.homeManagerModules.default
      ];

    };
    in
      {
      # Build darwin flake using:
      darwinConfigurations."James-MacBook-Personal" = nix-darwin.lib.darwinSystem {
        specialArgs = {
          user = "jameshobson";
          homedir = "/Users/jameshobson";
        };
        modules = [ 
          # Set Git commit hash for darwin-version.
          ({...}: { system.configurationRevision = self.rev or self.dirtyRev or null; })
          { nixpkgs.hostPlatform = "x86_64-darwin"; }
          ./system/macos
          nix-homebrew.darwinModules.nix-homebrew { 
            nix-homebrew = {
              enable = true;
              enableRosetta = false;
              user = "jameshobson";
            };
          }
          home-manager.darwinModules.home-manager (homeConf "jameshobson")
          mac-app-util.darwinModules.default
        ];
      };

      nixosConfigurations."macvm" = nixpkgs.lib.nixosSystem {
        modules = [
          ./system/linux
          ./system/linux/wm.nix
          ./system/linux/hardware/macvm.nix
          ({pkgs,...}: { environment.systemPackages = with pkgs; [
              firefox
              cambalache
            ];
          })
          home-manager.nixosModules.home-manager {
            home-manager.extraSpecialArgs = { username = "james"; };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.james = { 
              imports = [
                ./home.nix
                ./linux.nix
              ];
            };
          }
        ];
      };

      nixosConfigurations."nixos" = nixpkgs.lib.nixosSystem {
        modules = [
          ./system/linux
          ./system/linux/wm.nix
          ./system/linux/hardware/laptop.nix
          home-manager.nixosModules.home-manager {
            home-manager.extraSpecialArgs = { username = "james"; };
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.james = { 
              imports = [
                ./home.nix
                ./linux.nix
                ./desktop.nix
                { editors.emacs.obsidianDir = "~/Obsidian/Notes"; }
              ];
            };
          }
        ];
      };

      darwinConfigurations."htfdgm67md" = nix-darwin.lib.darwinSystem {
        specialArgs = {
          user = "james.hobson";
          homedir = "/Users/james.hobson";
        };
        modules = [ 
          # Set Git commit hash for darwin-version.
          ({...}: { system.configurationRevision = self.rev or self.dirtyRev or null; })
          { nixpkgs.hostPlatform = "aarch64-darwin"; }
          ./system/macos
          nix-homebrew.darwinModules.nix-homebrew { 
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = "james.hobson";
            };
          }
          home-manager.darwinModules.home-manager (homeConf "james.hobson")
          mac-app-util.darwinModules.default
        ];
      };
      homeConfigurations = {
        "rpi5-james" = let 
          pkgs = import nixpkgs { 
            system = "aarch64-linux";
            config.allowUnfree = true;
          };
        in home-manager.lib.homeManagerConfiguration {
            pkgs = pkgs;
            extraSpecialArgs = {
              username = "james";
            };
            modules = [
              ./home.nix
              ./linux.nix
              { 
                home.packages = [ pkgs.firefox ];
                imports = [ ./modules/emacs ];
                editors.emacs.enable = true;
              }
            ];
          };

        "helios64" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "aarch64-linux"; };
          extraSpecialArgs = {
            username = "james";
          };
          modules = [
            ./home.nix
            ./linux.nix
          ];
        };

        "leedsPC" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          extraSpecialArgs = {
            username = "james";
          };
          modules = [
            ./home.nix
            ./linux.nix
            ./desktop.nix
            ./graphical.nix
          ];
        };
      };
    };
}

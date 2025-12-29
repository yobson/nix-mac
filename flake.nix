{
  description = "James's Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    #home-manager.url = "github:nix-community/home-manager";
    home-manager.url = "github:yobson/home-manager?ref=xmonad-options";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    mac-app-util.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.inputs.cl-nix-lite.url = "github:r4v3n6101/cl-nix-lite/url-fix";
    nix-rosetta-builder = {
      url = "github:cpick/nix-rosetta-builder";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-homebrew, home-manager, mac-app-util, nix-rosetta-builder }:
    let homeConf = user: {
      home-manager.extraSpecialArgs = {
        username = user;
        roles = ["gui" "apps"];
      };
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.${user} = {
        imports = [ ./home.nix ];
      };
      home-manager.backupFileExtension = "hm-backup";
      home-manager.sharedModules = [
        mac-app-util.homeManagerModules.default
      ];

      # Optionally, use home-manager.extraSpecialArgs to pass
      # arguments to home.nix
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
          ./amd64.nix
          nix-homebrew.darwinModules.nix-homebrew { 
            nix-homebrew = {
              enable = true;
              enableRosetta = false;
              user = "jameshobson";
            };
          }
          home-manager.darwinModules.home-manager (homeConf "jameshobson")
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
          ./aarch64.nix
          nix-homebrew.darwinModules.nix-homebrew { 
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = "james.hobson";
            };
          }
          home-manager.darwinModules.home-manager (homeConf "james.hobson")
          nix-rosetta-builder.darwinModules.default {
            # see available options in module.nix's `options.nix-rosetta-builder`
            nix-rosetta-builder.onDemand = true;
          }
        ];
      };
      homeConfigurations = {
        "nixos" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          extraSpecialArgs = {
            username = "james";
            roles = ["gui" "apps"];
          };
          modules = [ ./home.nix ]; # Defined later
        };
        "rpi5-james" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "aarch64-linux"; };
          extraSpecialArgs = {
            username = "james";
            roles = ["gui"];
          };
          modules = [ ./home.nix ]; # Defined later
        };
        "helios64" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "aarch64-linux"; };
          extraSpecialArgs = {
            username = "james";
            roles = [];
          };
          modules = [ ./home.nix ]; # Defined later
        };
      };
    };
}

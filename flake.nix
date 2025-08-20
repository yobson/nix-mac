{
  description = "James's Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    mac-app-util.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-homebrew, home-manager, mac-app-util }:
  let homeConf = user: {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${user} = import ./home.nix;
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
    darwinConfigurations."James-MacBook-work" = nix-darwin.lib.darwinSystem {
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
        ];
      };
  };
}

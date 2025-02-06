{
  description = "James's Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-homebrew, home-manager }:
  let homeConf = {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.jameshobson = import ./home.nix;
        home-manager.backupFileExtension = "hm-backup";

        # Optionally, use home-manager.extraSpecialArgs to pass
        # arguments to home.nix
      };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .
    darwinConfigurations."tiddles" = nix-darwin.lib.darwinSystem {
        modules = [ 
          # Set Git commit hash for darwin-version.
          ({...}: { system.configurationRevision = self.rev or self.dirtyRev or null; })
          ./aarch64.nix
          nix-homebrew.darwinModules.nix-homebrew { 
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = "jameshobson";
              autoMigrate = true;
            };
          }
          home-manager.darwinModules.home-manager homeConf
        ];
      };

    darwinConfigurations."James-MacBook-Personal" = nix-darwin.lib.darwinSystem {
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
          home-manager.darwinModules.home-manager homeConf
        ];
      };
  };
}

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
  let homebrewConf = {
        enable = true;
        enableRosetta = true;
        user = "jameshobson";
        autoMigrate = true;
      }; 
      homeConf = {
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
    darwinConfigurations."MacBook-Pro-2" = nix-darwin.lib.darwinSystem {
      modules = [ 
        # Set Git commit hash for darwin-version.
        ({...}: { system.configurationRevision = self.rev or self.dirtyRev or null; })
        ./configuration.nix
        nix-homebrew.darwinModules.nix-homebrew { nix-homebrew = homebrewConf; }
        home-manager.darwinModules.home-manager homeConf
      ];
    };

    darwinPackages = self.darwinConfigurations."MacBook-Pro-2".pkgs;
  };
}

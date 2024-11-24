{
  description = "James's Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-homebrew }:
  let homebrewConf = {
      enable = true;
      enableRosetta = true;
      user = "jameshobson";
      autoMigrate = true;
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
      ];
    };

    darwinPackages = self.darwinConfigurations."MacBook-Pro-2".pkgs;
  };
}

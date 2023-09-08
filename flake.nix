{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlays.default ];
      };
    in with pkgs; {
      overlays.default = self: super:
        let
          hpkgs = super.haskellPackages;
          uusi = hpkgs.callCabal2nix "uusi" ./. { };
        in with super;
        with haskell.lib; {
          inherit uusi;
          uusi-dev =
            addBuildTools uusi [ cabal-install haskell-language-server ];
        };
      packages.x86_64-linux.default = uusi;
      devShells.x86_64-linux.default = uusi-dev.envFunc { };
    };
}

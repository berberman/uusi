{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlay ];
      };
    in with pkgs; {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          uusi = hpkgs.callCabal2nix "uusi" ./. { };
        in with super;
        with haskell.lib; {
          inherit uusi;
          uusi-dev =
            addBuildTools uusi [ haskell-language-server cabal-install ];
        };
      defaultPackage.x86_64-linux = uusi;
      devShell.x86_64-linux = uusi-dev.envFunc { };
    };
}

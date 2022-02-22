{
  description = "A very basic flake";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShell = with pkgs; mkShell {
        buildInputs = [ zlib ];
        nativeBuildInputs = [
          bashInteractive
          ghc
          cabal-install
          hlint
          haskellPackages.cabal-fmt
        ];
      };
    });
}

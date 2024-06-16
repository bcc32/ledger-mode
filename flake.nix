{
  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-darwin;
    in
    {
      devShells.x86_64-darwin.default = pkgs.mkShell { buildInputs = with pkgs; [ getopt ]; };
    };
}

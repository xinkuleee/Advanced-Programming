let
  # Pinning nixpkgs for reproducibility. If you want to use your
  # system Nixpkgs, use the other definition of 'nixpkgs' that is
  # commented out below.
  nixpkgs = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz;
    sha256 = "sha256:1915r28xc4znrh2vf4rrjnxldw2imysz819gzhk9qlrkqanmfsxd";
  };
  # nixpkgs = <nixpkgs>;

  pkgs = import nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "AP2025";
  buildInputs =
    with pkgs;
    [
      haskell.compiler.ghc98
      cabal-install
      haskell-language-server
    ];
}

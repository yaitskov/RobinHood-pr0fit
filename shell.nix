{}:
let
  d = import ./default.nix {};
  pkgs = d.pkgs;
  hsPkgs = d.hsPkgs;
  ghc = d.ghc;
  hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });
in
hsPkgs.shellFor {
  packages = p: [ p.robin-hood-profit ];
  nativeBuildInputs = (with pkgs; [
    cabal-install
    ghcid
    hlint
    niv
  ]) ++ [ hls hsPkgs.upload-doc-to-hackage ];
  shellHook = ''
      export PS1='$ '
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
    '';
}

{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  inherit (pkgs) lib;
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  sources = [
    "^src.*$"
    "^exe.*$"
    "^test.*$"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "robin-hood-profit" (lib.sourceByRegex ./. sources) { };
  robin-hood-profit-overlay = _hf: _hp: { robin-hood-profit = base; };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay robin-hood-profit-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });


  robin-hood-profit = hsPkgs.robin-hood-profit;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit robin-hood-profit;
}

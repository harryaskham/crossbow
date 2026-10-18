# GHC 9.10 with crossbow's dependencies (crossbow.cabal), from one pinned nixpkgs revision,
# used by capture.py to build the interpreter offline.
let
  src = builtins.fetchTree {
    type = "github"; owner = "NixOS"; repo = "nixpkgs";
    rev = "c043004d1c6985732bcc1cbc5a9c9aecbbb4e0f0";
    narHash = "sha256-7QYjT2vHLuX9Z1pdxHXDKCbh1CR3D/2rywB9Tx0MPRg=";
  };
  pkgs = import src { config.allowBroken = true; };
  hl = pkgs.haskell.lib.compose;
  hp = pkgs.haskellPackages.override {
    overrides = self: super: { bitwise = hl.dontCheck (hl.doJailbreak (hl.markUnbroken super.bitwise)); };
  };
in hp.ghcWithPackages (p: with p; [
  relude haskeline parsec vector array extra text containers mtl bimap mod template-haskell safe linear lens
  bitwise random split HUnit
])

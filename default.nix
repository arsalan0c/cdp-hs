{ compiler ? "ghc921" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "cdp-hs" =
        hself.callCabal2nix
          "cdp-hs"
          (gitignore ./.)
          {}
          "cdp"
          ;
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."cdp-hs"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."cdp-hs");

  docker = pkgs.dockerTools.buildImage {
    name = "cdp-hs";
    config.Cmd = [ "${exe}/bin/cdp-hs" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "cdp-hs" = myHaskellPackages."cdp-hs";
}

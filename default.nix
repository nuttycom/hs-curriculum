let 
  pkgs = import ./nix/pkgs.nix;

  hs-pkgs = pkgs.haskell.packages.ghc843;

  hs-curriculum = hs-pkgs.callCabal2nix "hs-curriculum" ./. {};

  dev = hs-curriculum.env.overrideAttrs (_: {

    buildInputs = with pkgs; with hs-pkgs; hs-curriculum.env.buildInputs ++ 
      [ cabal-install ghcid ];  
      
    shellHook = ''
      ${pkgs.figlet}/bin/figlet "land of haskell" | ${pkgs.lolcat}/bin/lolcat
      alias ghcid-loh="ghcid --command=\"cabal repl\""
      alias loh-repl="cabal repl"
      echo "tools available"
      echo "   cabal, ghcid, ghcid-loh, loh-repl"
    '';
  });
in
  if pkgs.lib.inNixShell then dev else hs-curriculum


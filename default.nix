let 
  pkgs = import ./nix/pkgs.nix;

  hs-pkgs = pkgs.haskell.packages.ghc843;

  hs-curriculum = hs-pkgs.callCabal2nix "hs-curriculum" ./. {};

  opener = if builtins.isList (builtins.match ".*darwin" "${builtins.currentSystem}") 
             then "open" 
             else "chromium-browser";
      
  dev = hs-curriculum.env.overrideAttrs (env: {
    buildInputs = with pkgs; with hs-pkgs; env.buildInputs ++ 
      [ cabal-install ghcid ];  

    shellHook = ''
      ${pkgs.figlet}/bin/figlet "land of haskell" | ${pkgs.lolcat}/bin/lolcat

      alias ghcid-loh="ghcid --command=\"cabal repl\""
      alias loh-repl="cabal repl"
      alias loh-doc="cabal haddock"
      alias loh-doc-open="cabal haddock && ${opener} dist/doc/html/hs-curriculum/Introlude.html"

      echo "tools available"
      echo "   cabal, ghcid, ghcid-loh, loh-repl, loh-doc, loh-doc-open"
    '';
  });
in
  if pkgs.lib.inNixShell then dev else hs-curriculum


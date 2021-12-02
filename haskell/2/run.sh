#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ghc haskellPackages.attoparsec
ghc main.hs
./main

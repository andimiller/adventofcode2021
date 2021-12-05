#!/usr/bin/env bash
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.attoparsec])"

let
  pkgs = import (builtins.fetchGit {
    name = "emacs-revision-28.2";
    url = "https://github.com/NixOS/nixpkgs/";
    rev = "bca7aaf8ea4c99c16c257a0e4f2e8384f28fb2cf";
  }) {};
  emacsWithPackages = with pkgs; (emacsPackagesFor emacs).emacsWithPackages;
  run-test = import ./run-test.nix {
    inherit emacsWithPackages;
  };
in run-test

{ emacsWithPackages }:
let
  pkgs = import <nixpkgs> {};
  versioned_emacs = emacsWithPackages (epkgs: with epkgs; [
    eglot
    ert-async
    el-mock
    ert-runner
    uuid
    deferred
    ht
    s
    dash
  ]);
in derivation rec {
  name = "eglot-copilot";
  baseInputs = [];
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  setup = ./setup.sh;
  buildInputs = [
    versioned_emacs pkgs.coreutils];
  emacs = versioned_emacs;
  eglot_copilot = ../eglot-copilot.el;
  test_target = ../test;
  system = builtins.currentSystem;
}

  

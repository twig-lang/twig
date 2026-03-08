{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
    gnumake
  ] ++ (with pkgs.ocamlPackages; [
    ocaml
    findlib
    odoc

    menhir
    menhirLib
    sedlex

    cmdliner
    alcotest
  ]);
}

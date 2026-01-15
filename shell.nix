{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
    gnumake
  ] ++ (with pkgs.ocamlPackages; [
    ocaml
    ocp-indent
    ocamlformat
    ocaml-lsp
    findlib
    odoc

    menhir
    menhirLib
    sedlex

    cmdliner
    alcotest
  ]);
}

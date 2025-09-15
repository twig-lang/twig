{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
  ] ++ (with pkgs.ocamlPackages; [
    ocaml
    ocp-indent
    ocamlformat
    ocaml-lsp
    findlib

    menhir
    menhirLib
    sedlex

    cmdliner
  ]);
}

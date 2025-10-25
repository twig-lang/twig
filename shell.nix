{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
    gnumake
    wabt
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
    wasm
  ]);
}

{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    dune_3
  ] ++ (with pkgs.ocamlPackages; [
    ocaml
    ocp-indent
    ocaml-lsp
    findlib
  ]);
}

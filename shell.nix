{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "aoc ocaml";
  buildInputs = with pkgs; [
    ocaml
    opam
    ocamlPackages.findlib 
    ocamlPackages.batteries
    ocamlPackages.ocaml-lsp
    dune_3
  ];
  
  shellHook = ''
    zsh
  '';
}

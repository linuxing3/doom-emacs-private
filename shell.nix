{ inputs, ... }@flakeContext:
{ system }:
let
  pkgs = inputs.nixpkgs.legacyPackages."${system}";
in
pkgs.mkShell {
  packages = with pkgs; [
    nixd
    nixfmt-rfc-style
    alejandra
    nil

    cmake

    shfmt
    shellcheck

    aider-chat
  ];
}

{ inputs, ... }@flakeContext:
{ system }:
let
  pkgs = inputs.nixpkgs.legacyPackages."${system}";
in
pkgs.mkShell {
  packages = with pkgs; [
    nixd
    nil
    aider-chat
  ];
}

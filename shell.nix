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
  shellHook = ''
    export DEEPSEEK_API_KEY=$(cat /etc/agenix/deepseek-token)
    aider
 '';
}

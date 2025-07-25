{
  description = "doom emacs with flake ai powered";
  inputs = {
    nixpkgs.url = "flake:nixpkgs/nixpkgs-unstable";
  };
  outputs =
    inputs:
    let
      flakeContext = {
        inherit inputs;
      };
    in
      {
      devShells = {
        x86_64-linux = {
          default = import ./shell.nix flakeContext { system = "x86_64-linux"; };
        };
      };
      packages = {
        x86_64-linux = {
          default = import ./package.nix flakeContext { system = "x86_64-linux"; };
        };
      };
    };
}

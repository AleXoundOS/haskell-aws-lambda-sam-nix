{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            helloProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc928";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
                # when LESS is set, aws CLI is not able to produce colored text
                shell.shellHook = "unset LESS";
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  # AWS CLI to upload zip Lambda bundle and manage AWS services
                  awscli2
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.helloProject.flake { };
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."haskell-aws-lambda-sam-nix:exe:haskell-aws-lambda-sam-nix";
        aws-lambda = import ./make-aws-lambda-zip.nix { inherit pkgs flake; };
      });
}

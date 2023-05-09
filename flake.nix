{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: rec {
        rsi-break = final.haskellPackages.callPackage (import ./default.nix) {};
        rsi-break-clean = with final.haskell.lib; dontCheck (justStaticExecutables rsi-break);
      });
      packages = forAllSystems (system: {
         rsi-break = nixpkgsFor.${system}.rsi-break;
         rsi-break-clean = nixpkgsFor.${system}.rsi-break-clean;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.rsi-break-clean);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.rsi-break];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}

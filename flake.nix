{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, flake-utils, nixpkgs, nix-filter }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPackages = pkgs.ocamlPackages;
      in with ocamlPackages; rec {
        defaultPackage = buildDunePackage {
          pname = "cerise-interpreter";
          version = "0.0.0";
          duneVersion = "3";

          src = with nix-filter.lib;
            nix-filter {
              root = ./.;
              include = [
                "dune-project"
                (inDirectory "src")
                (inDirectory "lib")
                (inDirectory "tests")
              ];
            };

          nativeBuildInputs = [ menhir ];
          buildInputs = [ containers notty zarith ];
          checkInputs = [ alcotest ];

          doCheck = true;

          meta = with pkgs.lib; {
            description =
              "Cerise interpreter, interpreter for capability machines";
            homepage = "https://github.com/logsem/cerise-interpreter";
            license = licenses.bsd3;
          };
        };

        devShell = pkgs.mkShell {
          packages = (with defaultPackage; [ buildInputs nativeBuildInputs ])
            ++ [ merlin ocaml-lsp ocamlformat ];

          inputsFrom = [ defaultPackage ];
        };

        formatter = pkgs.nixfmt;
      });
}

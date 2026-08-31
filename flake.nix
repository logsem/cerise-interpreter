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
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
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
                "Makefile"
                "flake.nix"
                (inDirectory "case_studies")
                (inDirectory "src")
                (inDirectory "lib")
                (inDirectory "tests")
              ];
            };

          nativeBuildInputs = [ pkgs.bash pkgs.gnumake pkgs.python3 pkgs.ripgrep pkgs.util-linux menhir ];
          postPatch = ''
            patchShebangs lib/backends/griotte_extracted/scripts
          '';
          buildInputs = [ containers menhirLib notty-community zarith ];
          checkInputs = [ alcotest ];

          doCheck = true;
          checkPhase = ''
            runHook preCheck
            make test
            runHook postCheck
          '';

          meta = with pkgs.lib; {
            description =
              "Cerise interpreter, interpreter for capability machines";
            homepage = "https://github.com/logsem/cerise-interpreter";
            license = licenses.bsd3;
          };
        };

        devShell = pkgs.mkShell {
          inputsFrom = [ defaultPackage ];

          packages = [
            merlin
            ocaml-lsp
            ocamlformat
          ];
        };

        formatter = pkgs.nixfmt;
      });
}

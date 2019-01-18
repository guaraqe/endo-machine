let
  nixpkgsSrc = builtins.fetchGit {
    name = "nixpkgs-unstable";
    url = https://github.com/NixOS/nixpkgs-channels/;
    ref = "nixos-unstable";
    rev = "be445a9074f139d63e704fa82610d25456562c3d";
  };

  overlay = _: pkgs:  {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = _: hspkgs: {
        generic-lens = pkgs.haskell.lib.dontCheck
          (hspkgs.callHackage "generic-lens" "1.0.0.2" {});
      };
    };
  };
in
  import nixpkgsSrc {
    overlays = [ overlay ];
  }

{self, lib, config, pkgs-unstable, inputs,...}:
let use_private_cache = !(config.archerd ? binary-cache && config.archerd.binary-cache.enable);
in {
  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [
        "archerd"
      ];
      substituters = [
        "https://cache.nixos.org"
        "https://cache.nixos-cuda.org"
      ] ++ lib.optional use_private_cache "https://cache.ts.archerdef.dev"
        ;
      trusted-public-keys = [
        "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
      ] ++ lib.optional use_private_cache "cache.ts.archerdef.dev:XCmNx437xEW1aU2mJ+5oq6svPdnQfjTWcIvMqvXlZ6U=";
    };

    registry = rec {
      nixpkgs.flake = inputs.nixpkgs; # nixpkgs is the same as this flake's nixpkgs
      nixpkgs-unstable.flake = inputs.nixpkgs-unstable;
      np = nixpkgs; # shortcut for nixpkgs#
      def.flake = inputs.self; # for my packages I guess...
    };

    # use newer version of nix?
    #package = pkgs-unstable.nixVersions.nix_2_18; # 2.18 is the default on 23.11
  };
}

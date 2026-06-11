{lib, config, pkgs-unstable,...}:
let use_private_cache = !(config.archerd ? binary-cache && config.archerd.binary-cache.enable);
in {
  nix = {
    # enabling experimental features
    # (nix-command enables use of nix to run different things instead many nix-* commands)
    # (flakes enables flakes, which I don't really understand...)
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


    # use newer version of nix?
    #package = pkgs-unstable.nixVersions.nix_2_18; # 2.18 is the default on 23.11
  };
}

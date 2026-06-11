{lib, config, pkgs-unstable,...}:
{
  nix = {
    # enabling experimental features
    # (nix-command enables use of nix to run different things instead many nix-* commands)
    # (flakes enables flakes, which I don't really understand...)
    settings = lib.mkMerge [
      {
        experimental-features = [
          "nix-command"
          "flakes"
        ];
      }
      (lib.mkIf (!(config.archerd ? binary-cache && config.archerd.binary-cache.enable)) {
        substituters = [
          "https://binarycache.ts.archerdef.dev"
          "https://cache.nixos.org"
        ];
        trusted-public-keys = [
          "binarycache.ts.archerdef.dev:AGQ4JzvGYZA3YoBh9xJjTtRKJ6Fy1rKhH+WMU5NoFZA="
        ];
      })
    ];


    # use newer version of nix?
    #package = pkgs-unstable.nixVersions.nix_2_18; # 2.18 is the default on 23.11
  };
}

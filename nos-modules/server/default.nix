{lib, config, ...}:
{
  imports = [
    ./proxy.nix
    ./homepage-dashboard.nix
    ./open-home.nix
    ./databases.nix
    ./photos-server.nix
    ./video-server.nix
    ./ai.nix
  ];

  options = {
    #TODO: need to add options for the default host (by name and ip address)
    archerd.server = {
      host = lib.mkOption {
        type = lib.types.str;
        default = "lambda1";
      };
      ip_addr = lib.mkOption {
        type = lib.types.str;
        default = "100.86.81.50";
      };
      mount.enable = lib.mkEnableOption "mounting paths for services to find";
      mount.user_dirs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = ["Pictures"];
      };
      mount.data_dir_path = lib.mkOption {
        type = lib.types.str;
        default = "/home/archerd/data";
      };
      mount.data_dirs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [
          "Pictures"
          "Videos"
          "nvidia"
          "Music"
        ];
      };
    };
  };

  config = {
    archerd.homepage-server = "homepage";
    # archerd.homepage-custom-image = true;
    archerd.ai.enable = true;
    archerd.proxy.enable = true;
    archerd.proxy.public_ip = true;
    archerd.immich.public-proxy.enable = true;

    archerd.server.mount.enable = true;
    fileSystems = lib.mkIf config.archerd.server.mount.enable (builtins.listToAttrs (
      (map (dir: {
        name = "/mnt/archerd/" + dir;
        value = {
          device = "/home/archerd/" + dir;
          fsType = "none";
          options = [
            "bind"
            "ro"
          ];
        };
      })
        config.archerd.server.mount.user_dirs
      ) ++ (map (dir: {
        name = "/mnt/data/" + dir;
        value = {
          device = config.archerd.server.mount.data_dir_path + "/" + dir;
          fsType = "none";
          options = [
            "bind"
            "ro"
          ];
        };
      })
        config.archerd.server.mount.data_dirs
      )
    ));
  };
}

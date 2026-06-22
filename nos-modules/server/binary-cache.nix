{lib, pkgs, config, ...}: {
  options = {
    archerd.binary-cache = {
      enable = lib.mkEnableOption "a self-hosted binary cache";
      cache-type = lib.mkOption {
        description = "Whether to use a simple or more complex binary cache.";
        type = lib.types.enum [ "simple" "complex" ];
        default = "simple";
      };
    };
  };
  config = lib.mkIf config.archerd.binary-cache.enable (lib.mkMerge [
    (lib.mkIf (config.archerd.binary-cache.cache-type == "simple")
    {
      services.nix-serve = {
        enable = true;
        secretKeyFile = "/home/archerd/repos/dotfiles/nixserve-private-key.pem"; # TODO: create a key
        package = pkgs.nix-serve-ng;
        extraParams = "--priority 60"; # after cache.nixos.org (40) and cache.nixos-cuda.org (50)
      };

      archerd.proxy.virtualHosts."binary-cache" = {
        host.ts_subdomain = "cache";
        proxy_to.local_port = config.services.nix-serve.port;
      };
    })
    (lib.mkIf (config.archerd.binary-cache.cache-type == "complex") {
      # TODO: implement something using attic or something.
      warnings = [ "The complex binary cache option is not implemented!" ];
    })
  ]);
}

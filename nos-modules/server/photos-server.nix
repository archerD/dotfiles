{lib, pkgs, config, ...}:
{
  options = {
  };

  config = {
    services.ente = {
      web = {
        enable = false;
      };
      api = {
        enable = false;
      };
    };

    services.immich = let port = 2283; in {
      enable = true;

      # This is the tailscale address for lambda1!
      host = config.archerd.server.ip_addr;
      port = port;
      openFirewall = false;

      # null means use any acceleration devices
      accelerationDevices = null;

      machine-learning.enable = true;


      environment = {
        # IMMICH_TRUSTED_PROXIES = lib.strings.concatStringsSep "," [
        #   # must be actual IP addresses
        # ];
      };
      settings = #null
      #/*
      {
        server.externalDomain = "";
        job.videoConversion.concurrency = 3;
        ffmpeg.accel = "nvenc";
      }
      #*/
      ;
    };
    services.immich-public-proxy = {
      # TODO: may be helpful in future for sharing things.
      enable = true;
      immichUrl = "http://${config.services.immich.host}:${builtins.toString config.services.immich.port}";
      settings.ipp = {
        singleImageGallery = true;
      };
    };
    environment.systemPackages = [
      pkgs.immich-go
    ];
  };
}

{lib, pkgs, config, ...}:
{
  options = {
    archerd.immich.public-proxy.enable = lib.mkEnableOption "immich public proxy and corresponding configuration.";
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

    archerd.proxy.virtualHosts = {
      "Immich Photo Sharing" = lib.mkIf config.archerd.immich.public-proxy.enable {
        host.pub_subdomain = "photos";
        proxy_to.local_port = config.services.immich-public-proxy.port;
      };
      "Immich" = {
        host.ts_subdomain = "immich";
        proxy_to.host_port = config.services.immich.port;
        abbr = "IM";
      };
/** could use something like this to make immich available at the same url
  # Immich Public Proxy paths
  @public path /share /share/*
  handle @public {
    # Your IPP server and port
    reverse_proxy YOUR_SERVER:3000
  }

  # All other paths, require basic auth and send to Immich
  handle {
    basic_auth {
      user password_hash
    }
    # Your Immich server and port
    reverse_proxy YOUR_SERVER:2283
  }
*/
    };
    services.immich = {
      enable = true;

      # This is the tailscale address for lambda1!
      host = config.archerd.server.ip_addr;
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
        server.externalDomain = lib.mkIf config.archerd.immich.public-proxy.enable "https://${config.archerd.proxy.virtualHosts."Immich Photo Sharing".host.url}/";
        job.videoConversion.concurrency = 3;
        ffmpeg.accel = "nvenc";
      }
      #*/
      ;
    };
    services.immich-public-proxy = {
      enable = config.archerd.immich.public-proxy.enable;
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

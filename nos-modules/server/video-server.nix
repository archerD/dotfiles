{...}:
{
  services.plex = {
    enable = false;
  };
  services.jellyfin = {
    enable = true;
    openFirewall = true;
    # user = "archerd";
  };
  archerd.proxy.virtualHosts."Jellyfin" = {
    host.ts_subdomain = "jellyfin";
    proxy_to.local_port = 8096;
  };
}

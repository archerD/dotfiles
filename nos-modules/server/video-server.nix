{...}:
{
  services.plex = {
    enable = false;
  };
  services.jellyfin = {
    enable = true;
    openFirewall = true;
    user = "archerd";
  };
}

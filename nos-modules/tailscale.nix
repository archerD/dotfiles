{ lib, config, pkgs, pkgs-unstable, ... }:
{
  services.tailscale = {
    enable = true;
    package = pkgs-unstable.tailscale; # vunerability on nix 25.05 version of tailscale.
    # permitCertUid = lib.mkIf config.services.caddy.enable "caddy"; # configured in server/proxy.nix
    useRoutingFeatures = "both";
    # extraUpFlags = [ "--advertise-exit-node" ]; # this does nothing since there is no authKeyFile.
  };
}

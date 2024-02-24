{ config, pkgs, ... }:
{
    services.tailscale = {
        enable = true;
        useRoutingFeatures = "server";
        extraUpFlags = ["--advertise-exit-node"]; # this does nothing since there is no authKeyFile.
    };
}

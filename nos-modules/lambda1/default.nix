{lib, ...}:
{
    # lambda1 specific configuration.
    imports = [
        ./hardware-configuration.nix
        ../server
    ];

    networking.hosts = {
        # force lambda1 through the tailscale network ip address.
        "127.0.0.2" = lib.mkForce [];
        "tailscale0" = ["lambda1"];
    };

    networking.hostName = "lambda1";

    archerd.secure-boot = false;
}

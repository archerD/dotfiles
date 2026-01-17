{...}:
{
    # lambda1 specific configuration.
    imports = [
        ./hardware-configuration.nix
        ../server
    ];

    networking.hostName = "lambda1";

    archerd.secure-boot = false;
}

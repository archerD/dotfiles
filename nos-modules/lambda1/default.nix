{...}:
{
    # lambda1 specific configuration.
    imports = [
        ./hardware-configuration.nix
    ];

    networking.hostName = "lambda1";

    archerd.secure-boot = false;
}

{...}:
{
    # fractal specific configuration.
    imports = [
        ./hardware-configuration.nix
        ./windows-drives.nix
    ];

    networking.hostName = "fractal";

    archerd.secure-boot = true;
    # Changed from default /boot/
    boot.loader.efi.efiSysMountPoint = "/boot/efi";
}

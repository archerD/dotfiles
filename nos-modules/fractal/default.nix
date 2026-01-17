{...}:
{
    imports = [
        ./hardware-configuration.nix
        ./windows-drives.nix
    ];

    archerd.secure-boot = true;
    # Changed from default /boot/
    boot.loader.efi.efiSysMountPoint = "/boot/efi";
}

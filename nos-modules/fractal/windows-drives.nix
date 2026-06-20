# a file to mount my drives also accessible by windows.
{ config, pkgs, ... }:
{
  # file system
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/3268BD0B68BCCF3B";
    fsType = "ntfs";
  };

  fileSystems."/mnt/windows" = {
    device = "/dev/disk/by-uuid/ECC47676C476433E";
    fsType = "ntfs";
  };

  fileSystems."/mnt/nix-backup" = {
    device = "/dev/disk/by-uuid/b4f918f2-4fd2-46c2-bcdb-d53240efab50";
    fsType = "ext4";
  };
}

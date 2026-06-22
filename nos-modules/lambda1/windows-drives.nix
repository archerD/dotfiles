# a file to mount my drives also accessible by windows.
{ config, pkgs, ... }:
{
  fileSystems."/mnt/windows-data" = {
    device = "/dev/disk/by-uuid/3268BD0B68BCCF3B";
    fsType = "ntfs";
  };
  fileSystems."/mnt/nix-backup" = {
    device = "/dev/disk/by-uuid/b4f918f2-4fd2-46c2-bcdb-d53240efab50";
    fsType = "ext4";
  };
}

# a file to mount my drives also accessible by windows.
{ config, pkgs, ... }:
{
  # file system
  fileSystems."/mnt/data" = {
    device = "/dev/disk/by-uuid/3268BD0B68BCCF3B";
    fsType = "ntfs";
  };

  fileSystems."/mnt/windows" = {
    device = "/dev/disk/by-uuid/3AB02D53B02D1743";
    fsType = "ntfs";
  };
}

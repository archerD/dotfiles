# a file to mount my drives also accessible by windows.
{ config, pkgs, ... }:
{
  fileSystems."/mnt/windows" = {
    device = "/dev/disk/by-uuid/ECC47676C476433E";
    fsType = "ntfs";
  };
}

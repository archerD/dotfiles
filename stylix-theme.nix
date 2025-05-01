{
  config,
  pkgs,
  lib,
  ...
}:
{
  stylix.enable = true;
  # stylix.image = images/wallpapers/nix-wallpaper-stripes-logo.png;
  # stylix.image = images/wallpapers/nix-wallpaper-hexagonal.png;
  stylix.image = images/wallpapers/nix-wallpaper-gear.png;
  stylix.polarity = "dark";
  stylix.fonts.monospace = {
    package = pkgs.nerdfonts.override { fonts = [ "3270" "JetBrainsMono"]; };
    name = "JetBrains Mono Nerd Font";
  };
  stylix.fonts.sizes = {
    desktop = 12;
    applications = 13;
    terminal = 12;
    popups = 16;
  };
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/google-dark.yaml";
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/macintosh.yaml";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/windows-10.yaml";
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/primer-dark.yaml";
}

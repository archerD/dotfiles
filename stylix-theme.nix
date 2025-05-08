{
  config,
  pkgs,
  lib,
  ...
}:
{
  stylix = { 
    enable = true;
    # image = images/wallpapers/nix-wallpaper-stripes-logo.png;
    # image = images/wallpapers/nix-wallpaper-hexagonal.png;
    # image = images/nixos-dark-tiling.png;
    # image = images/nixos-dark-tiling-think.png;
    image = images/wallpapers/nix-wallpaper-gear.png;
    polarity = "dark";
    fonts.monospace = {
      package = pkgs.nerdfonts.override { fonts = [ "3270" "JetBrainsMono"]; };
      name = "JetBrains Mono Nerd Font";
    };
    fonts.sizes = {
      desktop = 12;
      applications = 13;
      terminal = 12;
      popups = 16;
    };

    ### themes
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/google-dark.yaml";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/macintosh.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/windows-10.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/primer-dark.yaml";

    ### cursors and icons
    cursor = {
      # name = "gruppled_white_lite";
      # package = pkgs.gruppled-white-lite-cursors;
      # name = "volantes_cursors";
      # package = pkgs.volantes-cursors;
      # name = "Vimix-cursors";
      # package = pkgs.vimix-cursors;
      # name = "Hackneyed";
      # package = pkgs.hackneyed;
      name = "WhiteSur-cursors";
      package = pkgs.whitesur-cursors;
      size = 32;
    };
  } // lib.optionalAttrs (!(lib ? nixosSystem))
    {
      # HACK: this is only a home-manager option, consider moving into a hm module
      iconTheme = lib.mkIf (builtins.hasAttr "archerd" config) {
        enable = true;
        package = pkgs.vimix-icon-theme;
        dark = "Vimix-Doder-dark";
        light = "Vimix-Doder";
        # package = pkgs.papirus-icon-theme;
        # dark = "Papirus-Dark";
        # light = "Papirus-Light";
        # package = pkgs.tela-icon-theme;
        # dark = "Tela-green-dark";
        # light = "Tela-green";
      };
    };
}

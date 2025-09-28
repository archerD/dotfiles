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
      package = pkgs.nerd-fonts.jetbrains-mono;
      name = "JetBrains Mono Nerd Font";
    };
    fonts.sizes = {
      desktop = 12;
      applications = 13;
      terminal = 12;
      popups = 16;
    };

    ### themes
    base16Scheme = let
      lightSchemes = [ "cupertino" "measured-light" "papercolor-light" "selenized-white" ];
      schemesNames = [ "macintosh" "tube" "google-dark" "primer-dark" "everforest-dark-hard" ];
    in "${pkgs.base16-schemes}/share/themes/${builtins.elemAt schemesNames 0}.yaml";

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

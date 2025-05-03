{
  inputs,
  lib,
  config,
  pkgs,
  pkgs-unstable,
  pkgs-mine,
  ...
}:
{

  ### programs without provided customization
  home.packages = with pkgs; [
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    sysz # systemctl tui

    # games!
    kobodeluxe
    tetrio-desktop
    bzflag

    # gui apps
    google-chrome

    pkgs-mine.radio-active # tui radio player
    unison # file syncing
    mosh # better ssh

    # useful cli tools
    pkgs-mine.clustergit
    fd
    ripgrep
    nix-inspect

    # misc packages/scripts
    bat
    timer
    (writeShellScriptBin "overlay" ''
      ${pkgs.kitty}/bin/kitty  -o background_opacity=0.5 -o font_size=20 -o enable_audio_bell=yes -o visual_bell_duration=1.5 --class kitty-overlay &
    '')
  ] ++ lib.optionals config.archerd.targetGenericLinux [
    inputs.system-manager.packages.${system}.system-manager
    nautilus

    # TODO: see if this can be removed? or move to xmonad-de.nix
    (haskellPackages.ghcWithPackages (self : [
      self.ghc
      self.xmobar # this line is needed to rebuild xmobar?
      self.xmonad
      self.xmonad-contrib
      self.xmonad-extras
    ]))
    xmobar.bin
  ];

  ### programs with customization
  programs.cmus = {
    enable = true;
    theme = "night";
  };

  programs.fastfetch = {
    enable = true;
  };

  programs.nh = {
    enable = config.archerd.targetGenericLinux;
    flake = "/home/archerd/repos/dotfiles";
  };
}

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
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    inputs.nix-index-database.hmModules.nix-index

    # You can also split up your configuration and import pieces of it here:
    ./nvim.nix
    ./xmonad-de.nix
    ./shell-terminal.nix
    ./programs.nix
  ];

  options.archerd = with lib; {
    # TODO: consider making this an enable option or something, like targetUbuntu or something.
    baseSystem = mkOption {
      type = types.enum [
        "ubuntu"
        "nixos"
      ];
      description = ''
        What the base system is, used to control things that depend on this, such as:
          * setting targets.genericLinux.enable
          * choosing the path for xsecurelock
      '';
    };
  };

  config = {
    nixpkgs = {
      # You can add overlays here
      overlays = [
        # If you want to use overlays exported from other flakes:
        # neovim-nightly-overlay.overlays.default

        # Or define it inline, for example:
        # (final: prev: {
        #   hi = final.hello.overrideAttrs (oldAttrs: {
        #     patches = [ ./change-hello-to-hi.patch ];
        #   });
        # })

      ];
      # Configure your nixpkgs instance
      config = {
        # Disable if you don't want unfree packages
        allowUnfree = true;
      };
    };

    targets.genericLinux = lib.mkIf (config.archerd.baseSystem != "nixos") { enable = true; };
    archerd.useLocalScreenLocker = config.archerd.baseSystem == "ubuntu";

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "archerd";
    home.homeDirectory = "/home/archerd";

    # TODO: move this to another module? maybe make ubuntu only?
    services.ssh-agent = {
      enable = true;
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "24.11"; # Please read the comment before changing.

    # Nicely reload system units when changing configs
    systemd.user.startServices = "sd-switch";

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # TODO: consider moving to using home.file/xdg.configFile over dotbot eventually.
    # TODO: package the install script somehow...
    home.activation = {
      # runDotBot = lib.hm.dag.entryAfter ["writeBoundary"] ''
      #     run ${builtins.toPath ../../install} --quiet $VERBOSE_ARG
      #   '';
    };
    # Home Manager is pretty good at managing dotfiles. The primary way to manage
    # plain files is through 'home.file'.
    home.file = {
      # # Building this configuration will create a copy of 'dotfiles/screenrc' in
      # # the Nix store. Activating the configuration will then make '~/.screenrc' a
      # # symlink to the Nix store copy.
      # ".screenrc".source = dotfiles/screenrc;

      # # You can also set the file content immediately.
      # ".gradle/gradle.properties".text = ''
      #   org.gradle.console=verbose
      #   org.gradle.daemon.idletimeout=3600000
      # '';
    };
  };
}

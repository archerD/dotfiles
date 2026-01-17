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

    # (python3.withPackages (self: [
    #   self.ipython
    #   self.coreapi
    # ]))

    sysz # systemctl tui

    # games!
    kobodeluxe
    tetrio-desktop
    bzflag
    _4d-minesweeper

    # gui apps
    google-chrome
    tageditor # to edit mp3 file metadata
    #TODO: integrate xeyes somewhere in config xmonad config? (keybind to display the program on screen briefly)
    xorg.xeyes # displays eyes that follow the cursor...
    cheese

    pkgs-mine.radio-active # tui radio player
    unison # file syncing
    mosh # better ssh

    # useful cli tools
    pkgs-mine.clustergit
    nix-inspect

    # misc packages/scripts
    timer
    (writeShellScriptBin "overlay" ''
      ${pkgs.kitty}/bin/kitty  -o background_opacity=0.5 -o font_size=20 -o enable_audio_bell=yes -o visual_bell_duration=1.5 --class kitty-overlay &
    '')
  ] ++ lib.optionals config.archerd.targetGenericLinux [
    inputs.system-manager.packages.${system}.default
    nautilus
  ];

  ### programs with customization
  programs.cmus = {
    enable = true;
    theme = "night";
  };

  programs.ranger = {
    enable = true;
    settings = {
      vcs_aware = true;

      preview_images = true;
      preview_images_method = "kitty";

      preview_script = "/home/archerd/.dotfiles/config-files/ranger-preview.sh";
    };

    extraPackages = with pkgs; [
      poppler-utils
      ffmpegthumbnailer
    ];
  };

  programs.mpv = {
    enable = true;
    scripts = let
      # uosc has better sample config
      custom-osc = pkgs.mpvScripts.uosc;
      # custom-osc = pkgs.mpvScripts.modernz;
    in  [
      pkgs.mpvScripts.mpv-playlistmanager
      pkgs.mpvScripts.mpris
      pkgs.mpvScripts.thumbfast
      pkgs.mpvScripts.eisa01.undoredo
      pkgs-mine.mpvScripts.toggle-shuffle
      custom-osc
    ];
    bindings = {
      "s" = "script-binding toggle-shuffle";
      ":" = "script-binding commands/open";
      "GO_FORWARD" = "playlist-next";
      "MBTN_LEFT" = "cycle pause";
      "MBTN_RIGHT" = "script-binding menu";
      "shift+tab" = "script-binding uosc/toggle-ui";
      "tab" = "script-message-to uosc flash-ui";
      "space" = "cycle pause; script-message-to uosc flash-ui";
    };
    config = {
      osc = "no";
      # border = "no";
    };
    # TODO: add the uosc and modernz config here and then enable stylix
    /* scriptOpts = {
      uosc = {
      };
      modernz = {
      };
    }; */
  };
  stylix.targets.mpv.enable = false;

  programs.ripgrep.enable = true;
  programs.fd.enable = true;
  programs.bat.enable = true;
  programs.watson.enable = true;

  programs.fastfetch = {
    enable = true;
  };

  programs.nh = {
    enable = config.archerd.targetGenericLinux;
    flake = "/home/archerd/repos/dotfiles";
  };
}

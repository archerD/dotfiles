{
  inputs,
  lib,
  config,
  pkgs,
  pkgs-unstable,
  pkgs-mine,
  ...
}:
rec {
  /*
    Scope: the things related to my terminal/shell configuration
    NOT tui programs and the like.
    so... kitty, bash, starship, other things that would go in my bash config files...
    some other path and variable things...
  */

  # TODO: consider moving this into it's own module, which can be extended to include any number of packages...
  nixGL = lib.mkIf (config.archerd.baseSystem == "ubuntu") {
    packages = inputs.nixGL.packages;
  };
  nixpkgs.overlays = lib.optionals (config.archerd.baseSystem == "ubuntu")
    [(self: super: {
      kitty = config.lib.nixGL.wrap super.kitty;
    })
  ];
  # kitty needs glx bindings, because its gpu accelerated...
  programs.kitty = {
    enable = true;
    # HACK: Can't really use the nix pkg on ubuntu, because it uses the gpu...
    # TODO: look into wrapping kitty with nixgl instead of passing the empty file...
    package =
      if config.archerd.baseSystem == "nixos"
        then pkgs.kitty
      else pkgs.emptyDirectory;
    # configured by stylix, will be overridden
    font = lib.mkDefault { size = 12; name = "JetBrains Mono Nerd Font"; };
    settings = {
      enable_audio_bell = false;
      visual_bell_duration = 1.0;
      # command_on_bell = ...; # not sure what it would be...
      # consider specifying what layouts to enable, maybe fat, grid, splits (not using horizontal, stack, tall, vertical)
      # enabled_layouts = "*";

      # tab bar
      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      # tab_powerline_style = "round";
      tab_powerline_style = "slanted";
      tab_bar_min_tabs = 1;

      # Theming stuff, will be overridden by stylix, if enabled
      # background_image = "/home/archerd/.dotfiles/images/nixos-dark-tiling.png";
      color0 = "#000000";
      color8 = "#7f7f7f";
      #: black
      color1 = "#cd0000";
      color9 = "#ff0000";
      #: red
      color2 = "#00cd00";
      color10 = "#00ff00";
      #: green
      color3 = "#cdcd00";
      color11 = "#ffff00";
      #: yellow
      color4 = "#0000ee";
      color12 = "#5c5cff";
      #: blue
      color5 = "#cd00cd";
      color13 = "#ff00ff";
      #: magenta
      color6 = "#00cdcd";
      color14 = "#00ffff";
      #: cyan
      color7 = "#e5e5e5";
      color15 = "#ffffff";
      #: white
    };
    keybindings = {
      # use the same cwd when opening new window/tab
      "kitty_mod+enter" = "launch --cwd=current";
      "kitty_mod+t" = "new_tab_with_cwd";
      # unmap some keybindings
      "kitty_mod+w" = "no_op";
      "kitty_mod+6" = "no_op";
    };
  };
  # stylix.targets.kitty.enable = false;
  # stylix.targets.kitty.variant256Colors = true; # the template for 256 is borked.
  warnings =
    if config.archerd.baseSystem != "nixos" then
      [ "Make sure to install kitty locally!" ]
    else
      [ ];

  # from the nix-index-database flake module.
  # provides nix-locate, comma, and hooks into the shell command not found hook
  programs.nix-index-database.comma.enable = true;
  programs.nix-index.enable = true;

  # Environment switcher (i.e., autoload stuff when entering directory)
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/archerd/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    TEST = "hello world";
  };

  home.sessionPath = [ "$HOME/bin" ];

  ### bash, starship
  programs.bash = {
    enable = true;
    shellAliases = {
      # alias to run hm installed tools as sudo
      sudo-hm = "sudo --preserve-env=PATH env";

      # tool aliases
      ls = "ls --color=auto -F";
      # dir="dir --color=auto";
      # vdir="vdir --color=auto";

      grep = "grep --color=auto";
      fgrep = "fgrep --color=auto";
      egrep = "egrep --color=auto";

      # some more ls aliases
      ll = "ls -alF";
      la = "ls -A";
      l = "ls -CF";

      # Add an "alert" alias for long running commands.  Use like so:
      #   sleep 10; alert
      alert = "${pkgs.libnotify}/bin/notify-send --urgency=low -i \"$([ $? = 0 ] && echo terminal || echo error)\" \"$(history|tail -n1|sed -e 's/^\\s*[0-9]\\+\\s*//;s/[;&|]\\s*alert$//')\"";

      # prevent accidentally clobering files
      mv = "mv -i";
      cp = "cp -i";

      # kitty related aliases
      kitty-ssh = "${pkgs.kitty}/bin/kitten ssh";
      icat = "${pkgs.kitty}/bin/kitten icat";
      kitty-update = "curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin";

      # alias for cmus so it is detachable
      # tmux version
      #alias cmus='tmux new-session -A -D -s cmus "$(which cmus)"'
      #alias cmus='tmux attach-session -t cmus || tmux new-session -A -D -s cmus "$(which cmus)"'
      # in cmus: :bind -f common q shell tmux detach-client -s cmus
      # abduco version, uses ctrl-z to detach (can change by replacing ^z below)
      # cmus = ''abduco -A -e ^z cmus "$(which cmus)"'';
      # now that I have a scratchpad for cmus, do I still need this alias?
    };
    historyControl = [
      "ignoredups"
      "ignorespace"
    ];
    sessionVariables = home.sessionVariables;
    shellOptions = [ "histappend" ];

    initExtra = ''
      ${pkgs.fastfetch}/bin/fastfetch
    '';
  };
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      format = lib.concatStrings [
        "$directory"
        "$all"
        "$username"
        "$hostname"
        "$character"
      ];
      add_newline = true;

      cmd_duration = {
        disabled = true;
        show_notifications = false;
      };

      directory = {
        truncation_length = 0;
        truncate_to_repo = false;
        style = "cyan";
        repo_root_style = "#f54d28";
      };

      shlvl = {
        disabled = false;
      };

      username = {
        show_always = true;
        format = "[$user]($style)@";
      };
      hostname = {
        ssh_only = false;
        ssh_symbol = " 🌐";
        format = "[$hostname$ssh_symbol]($style)";
      };

      git_metrics.disabled = false;
    };
  };
}

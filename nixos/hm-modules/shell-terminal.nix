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

  # kitty needs glx bindings, because its gpu accelerated...
  home.packages = lib.optionals (config.archerd.baseSystem == "nixos") [
    pkgs.kitty
  ];
  programs.kitty = {
    # TODO: configure this.
    enable = false;
  };

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

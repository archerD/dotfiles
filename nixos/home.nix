{ inputs, lib, config, pkgs, ... }:
rec {
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  /*
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
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };
  */

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "archerd";
  home.homeDirectory = "/home/archerd";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # example usages:

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

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
  ];

  # programs with builtin support...
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
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
    EDITOR = "nvim";
    TEST = "hello world";
  };

  #home.sessionPath = [ "$HOME/bin" ];

  programs.bash = {
    enable = true;
    shellAliases = {
      # tool aliases
      ls="ls --color=auto -F";
#      dir="dir --color=auto";
#      vdir="vdir --color=auto";

      grep="grep --color=auto";
      fgrep="fgrep --color=auto";
      egrep="egrep --color=auto";

      # some more ls aliases
      ll="ls -alF";
      la="ls -A";
      l="ls -CF";

      # Add an "alert" alias for long running commands.  Use like so:
      #   sleep 10; alert
      alert="notify-send --urgency=low -i \"$([ $? = 0 ] && echo terminal || echo error)\" \"$(history|tail -n1|sed -e '\\''s/^\\s*[0-9]\\+\\s*//;s/[;&|]\\s*alert$//'\\'')\"";

      # prevent accidentally clobering files
      mv = "mv -i";
      cp = "cp -i";

      # kitty related aliases
      kitty-update="curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin";
      kitty-ssh="kitty +kitten ssh";
      icat="kitty +kitten icat";
      
# alias for cmus so it is detachable
# tmux version
#alias cmus='tmux new-session -A -D -s cmus "$(which cmus)"'
#alias cmus='tmux attach-session -t cmus || tmux new-session -A -D -s cmus "$(which cmus)"'
# in cmus: :bind -f common q shell tmux detach-client -s cmus
# abduco version, uses ctrl-z to detach (can change by replacing ^z below)
      cmus="abduco -A -e ^z cmus \"$(which cmus)\"";
      # now that I have a scratchpad for cmus, do I still need this alias?
    };
    historyControl = [ "ignoredups" "ignorespace" ];
    sessionVariables = home.sessionVariables;
    shellOptions = [ "histappend" ];

    initExtra =
    ''
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if test -n "&KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
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

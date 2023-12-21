{ inputs, lib, config, pkgs, pkgs-unstable, ... }:
rec {
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    ./nvim.nix
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

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = 
      let zenlog = pkgs.python3Packages.buildPythonPackage rec {
          pname = "zenlog";
          version = "1.1";
          src = pkgs.fetchPypi {
              inherit pname version;
              sha256 = "83460a85fa7249b8007c03681a6a0b575ce6fe044349389d3d3d43f58d4687de";
          };
          doCheck = false;
          propagatedBuildInputs = with pkgs.python3Packages; [
              colorlog
          ];
      };
      radio-active = pkgs.python3Packages.buildPythonApplication rec {
          pname = "radio-active";
          version = "2.8.0";
          src = pkgs.fetchPypi {
              inherit pname version;
              sha256 = "7d01ce460cac3b57f421762c8943187ee3bd458e51985d9d15d37381b6fe265c";
          };
          doCheck = false;
          propagatedBuildInputs = with pkgs.python3Packages; [
              pkgs.ffmpeg_4-full
                  requests
                  urllib3
                  psutil
                  pyradios
                  requests-cache
                  rich
                  pick
                  zenlog

                  flake8
                  twine
                  black
          ];
      }
  ; in with pkgs; [
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

    # a slack/discord like communication tool
    zulip
    zulip-term

    # screenlocking...
    xsecurelock
    sysz # systemctl tui

    comma # run things without installing

    radio-active # tui radio player

    # an lsp for nix files
    pkgs-unstable.pkgs.nixd

    # misc packages/scripts
    pkgs.bat
    pkgs.timer
    (pkgs.writeShellScriptBin "overlay" ''
        kitty -o background_opacity=0.5 -o font_size=20 -o enable_audio_bell=yes -o visual_bell_duration=1.5 --class kitty-overlay &
    '')
  ];

  # screen locking experiment
  services.screen-locker = {
    enable = true;
    xautolock.enable = false;
    inactiveInterval = 15;

    xss-lock.extraOptions = [
        "--transfer-sleep-lock"
        "-n" "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
        ];
    # This seems to work, but is a bit weird, needs the path set..., otherwise xscreensaver can't use some utilities.
    lockCmd = ''
        /usr/bin/env PATH="/run/current-system/sw/bin/:$PATH"  XSECURELOCK_SAVER=saver_xscreensaver XSECURELOCK_AUTH_TIMEOUT=10 XSECURELOCK_KEY_XF86AudioPlay_COMMAND="playerctl -p playerctld play-pause" XSECURELOCK_KEY_XF86AudioPrev_COMMAND="playerctl -p playerctld previous" XSECURELOCK_KEY_XF86AudioNext_COMMAND="playerctl -p playerctld next" XSECURELOCK_KEY_XF86AudioStop_COMMAND="playerctl -p playerctld stop" XSECURELOCK_KEY_XF86AudioMute_COMMAND="amixer set Master toggle" XSECURELOCK_KEY_XF86AudioLowerVolume_COMMAND="amixer set Master 2%-" XSECURELOCK_KEY_XF86AudioRaiseVolume_COMMAND="amixer set Master 2%+" XSECURELOCK_PASSWORD_PROMPT="time" XSECURELOCK_SHOW_DATETIME=1 XSECURELOCK_DATETIME_FORMAT="(%%a) %%F T %%R:%%S%%z (%%Z)" ${pkgs.xsecurelock}/bin/xsecurelock
    '';
  };

  services.redshift = {
    enable = true;
    tray = true;
    provider = "geoclue2";
  };

  programs.rofi = {
    enable = true;

    theme = "fancy";
    font = "JetBrains Mono Nerd Font 16";
    terminal = "kitty";

    plugins = [ pkgs.rofi-calc pkgs.rofi-power-menu ];
    extraConfig = {
      modi = "filebrowser,ssh,window,run,drun,calc,keys";
      case-sensitive = false;
      ssh-client = "kitty +kitten ssh";
      #timeout = {
      #    action = "kb-cancel";
      #    delay = 0;
      #};
      #filebrowser = {
      #    directories-first = true;
      #    sorting-method = "name";
      #};
    };
  };

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
    TEST = "hello world";
  };

  home.sessionPath = [ "$HOME/bin" ];

  programs.bash = {
    enable = true;
    shellAliases = {
      # tool aliases
      ls="ls --color=auto -F";
      # dir="dir --color=auto";
      # vdir="vdir --color=auto";

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
    echo Bash managed by home-manager!
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


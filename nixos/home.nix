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
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    inputs.nix-index-database.hmModules.nix-index

    # You can also split up your configuration and import pieces of it here:
    ./nvim.nix
  ];

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

      # bump xmonad version to latest! is this necessary? no. but I can.
      (self: super: {
        haskellPackages = super.haskellPackages.override (old: {
          overrides = self.lib.composeExtensions (old.overrides or (_: _: { })) (
            hself: hsuper: {
              # xmonad = super.haskell.lib.appendPatch hsuper.xmonad_0_17_0 ./xmonad-nix.patch;
              xmonad = hsuper.xmonad_0_18_0;
              xmonad-contrib = hsuper.xmonad-contrib_0_18_1;
              # xmonad-extras = hsuper.xmonad-extras_0_17_0;
            }
          );
        });
      })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "archerd";
  home.homeDirectory = "/home/archerd";

  # manage xmonad in home-manager
  xsession = {
    enable = true;
    numlock.enable = true;
    # could also use multiline string ('' '') to put contents of the script here.
    # TODO: investigate why some programs aren't launching (notably, trayer).
    initExtra = builtins.readFile ../xlogin_script;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.List
        haskellPackages.monad-logger
        haskellPackages.xmonad
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmobar
      ];
      # either allow home-manager to manage xmonad directly (config is path),
      # or leave the file under dotbot control (config is null)
      # dotbot allows for self recompiling, hm means recompiling must be done via a hm switch
      # config = ../xmonad.hs;
      config = null;
      libFiles = {
        "HomeManagerProvided.hs" = pkgs.writeText "HomeManagerProvided.hs" ''
          module HomeManagerProvided where
          screenshot = ""
          kitty = "${pkgs.kitty}/bin/kitty "
          focusedBorder = "#772388"
          normalBorder = "#348823"
        '';
      };
    };
  };
  programs.xmobar = {
    enable = false;
    extraConfig = builtins.readFile ../xmobar.hs;
  };
  services.trayer = {
    enable = false; # TODO: set this service up, and remove from ../tray.sh
  };

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

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
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

    # for xmobar volume?
    alsa-utils

    # misc packages/scripts
    bat
    timer
    (writeShellScriptBin "overlay" ''
      kitty -o background_opacity=0.5 -o font_size=20 -o enable_audio_bell=yes -o visual_bell_duration=1.5 --class kitty-overlay &
    '')
  ];

  services.screen-locker = {
    enable = true;
    inactiveInterval = 15;

    xss-lock.extraOptions = [
      "--transfer-sleep-lock"
      "-n"
      "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
    ];

    xautolock = {
      enable = true;
      extraOptions = [
        "-notify"
        "15"
        "-notifier"
        "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
      ];
    };

    # Two options for this module on non nixos: use the htpasswd authproto or don't use nix version of xsecurelock.
    # Reason: PAM stuff... hard to exactly say why for me, but nix can't properly setup PAM,
    # because security stuff..., and also the nix version of PAM can't handle imports that
    # ubuntu has in it's version of pam services.  Even if it could, the nix version of PAM
    # does not have the modules that the services seem to want.
    lockCmd =
      let # the base version, to be used on nixos only
        # the addition of the PATH variable is so playerctl and amixer can be found.
        xsecurelock-base = "PATH=\"/run/current-system/sw/bin\" ${pkgs.xsecurelock}/bin/xsecurelock";
        # to use the htpasswd, need to generate a file, `( umask 077; htpasswd -cB ~/.xsecurelock.pw "$USER" )`, see https://github.com/google/xsecurelock#authentication-protocol-modules
        xsecurelock-htpasswd = "XSECURELOCK_AUTHPROTO=authproto_htpasswd " + xsecurelock-base;
        # need to install xsecurelock outside of nixos if using this.
        xsecurelock-local = "/usr/local/bin/xsecurelock";
      in
      ''/usr/bin/env XSECURELOCK_SAVER=saver_xscreensaver XSECURELOCK_AUTH_TIMEOUT=10 XSECURELOCK_KEY_XF86AudioPlay_COMMAND="playerctl -p playerctld play-pause" XSECURELOCK_KEY_XF86AudioPrev_COMMAND="playerctl -p playerctld previous" XSECURELOCK_KEY_XF86AudioNext_COMMAND="playerctl -p playerctld next" XSECURELOCK_KEY_XF86AudioStop_COMMAND="playerctl -p playerctld stop" XSECURELOCK_KEY_XF86AudioMute_COMMAND="amixer set Master toggle" XSECURELOCK_KEY_XF86AudioLowerVolume_COMMAND="amixer set Master 2%-" XSECURELOCK_KEY_XF86AudioRaiseVolume_COMMAND="amixer set Master 2%+" XSECURELOCK_PASSWORD_PROMPT="time" XSECURELOCK_SHOW_DATETIME=1 XSECURELOCK_DATETIME_FORMAT="(%%a) %%F T %%R:%%S%%z (%%Z)" ''
      + xsecurelock-base;
  };

  services.redshift = {
    enable = true;
    tray = true;
    provider = "geoclue2";
  };

  # starts the service, but cannot open the firewall
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  # from the nix-index-database flake module.
  programs.nix-index-database.comma.enable = true;
  programs.nix-index.enable = true;

  programs.rofi = {
    enable = true;

    theme = "fancy";
    font = "JetBrains Mono Nerd Font 16";
    terminal = "kitty";

    plugins = [
      pkgs.rofi-calc
      pkgs.rofi-power-menu
    ];
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

  programs.cmus = {
    enable = true;
    theme = "night";
  };

  programs.bash = {
    enable = true;
    shellAliases = {
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
      alert = "notify-send --urgency=low -i \"$([ $? = 0 ] && echo terminal || echo error)\" \"$(history|tail -n1|sed -e '\\''s/^\\s*[0-9]\\+\\s*//;s/[;&|]\\s*alert$//'\\'')\"";

      # prevent accidentally clobering files
      mv = "mv -i";
      cp = "cp -i";

      # kitty related aliases
      kitty-update = "curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin";
      kitty-ssh = "kitty +kitten ssh";
      icat = "kitty +kitten icat";

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
      fastfetch
    '';
  };

  programs.fastfetch = {
    enable = true;
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

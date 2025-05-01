{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
{
  /*
    scope of this file:
    all things one could call the desktop environment...
    so: xmonad, xmobar, trayer (and contents), screen locker, rofi, compositor, ...
    Adding in, but may move xresources
    maybe others things, but not sure yet...
  */

  options = with lib; {
    # TODO: consider making this a nullOr path type, which specifies the path to the screen locker.
    archerd.useLocalScreenLocker = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use the locally installed screen locker.
        This is useful for non nixos systems, because some PAM stuff doesn't play nice,
        which understanding exactly why is above my paygrade.
        Assumes using xsecurelock, installed at /usr/local/bin/xsecurelock.
      '';
    };
    archerd.highResolutionScreen = mkEnableOption "settings for high resolutions screens (larger fonts, etc.)";
  };

  config = {
    ### overlay! (to set xmonad to highest version, because I can...)
    nixpkgs.overlays = [
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

    ### xresources (some monitor stuff)
    #TODO: consider if this is the best place for this.
    xresources.properties = lib.mkMerge [
      (lib.mkIf config.archerd.highResolutionScreen {
        "Xft.dpi" = 161; # manually calculated for my high res screen
        # TODO: consider using xrandr to only scale the one monitor needed.
        # probably by adding to an autorandr hook (something like `xrandr --output DP-2 --scale 1.5x1.5`)
      })
      {
        # These might also be useful depending on your monitor and personal preference:
        "Xft.autohint" = 0;
        "Xft.lcdfilter" = "lcddefault";
        "Xft.hintstyle" = "hintfull";
        "Xft.hinting" = 1;
        "Xft.antialias" = 1;
        "Xft.rgba" = "rgb";
      }
    ];

    ### XMonad & XMobar!
    xsession = {
      enable = true;
      numlock.enable = true;
      # could also use multiline string ('' '') to put contents of the script here.
      # TODO: investigate why some programs aren't launching (notably, trayer).
      initExtra = builtins.readFile ../scripts/xlogin_script;
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
        libFiles = let 
          kittyPath = if config.archerd.baseSystem == "nixos"
            then "${pkgs.kitty}/bin/kitty"
          else "~/bin/kitty";
        in 
          {
          "HomeManagerProvided.hs" = pkgs.writeText "HomeManagerProvided.hs" ''
            module HomeManagerProvided where
            screenshot = ""
            kitty = "${kittyPath} "
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
    home.packages = [
      # for xmobar volume?
      pkgs.alsa-utils
    ];
    # a library file for xmobar!
    xdg.configFile."xmobar/lib/HomeManagerProvided.hs" =
      let
        highResSwitch = high: low: toString (if config.archerd.highResolutionScreen then high else low);
      in
      {
        text = ''
          module HomeManagerProvided where
          fontSize = "${highResSwitch 18 11}"
          additionalFontSizes = ["${highResSwitch 16 12}"]
          topSizeHeight :: Int
          topSizeHeight = ${highResSwitch 38 26}
        '';
        # the issue is that the date given is basically epoch, for build determinisim...
        # we can't set the date for the home manager file, but we can create a new file with touch,
        # so that the date is current date, which is enough to tell xmobar to recompile itself
        onChange = "touch ${config.xdg.configHome}/xmobar/lib/recompileFlag.hs";
      };

    ### The tray stuff!
    services.trayer = {
      enable = true;
      settings = lib.mkMerge [
        {
          edge = "top";
          align = "right";
          widthtype = "request";
          SetDockType = true;
          SetPartialStrut = true;
          transparent = true;
          alpha = 0;
          tint = "0x294029";
          # alpha = 63;
          # tint = "0x145A32";
          expand = true;
          padding = 3;
          monitor = "primary";
          iconspacing = 2;
        }
        (lib.mkIf config.archerd.highResolutionScreen {
          heighttype = "pixel";
          height = 38;
        })
        (lib.mkIf (!config.archerd.highResolutionScreen) {
          heighttype = "request";
        })
      ];
    };

    ## the tray applets: redshift, kdeconnect, nm-applet, blueman-applet, pasystray
    ## others started by the tray.sh script: xfce-power-manager, system-config-printer-applet, and a yubikey applet
    services.pasystray.enable = true;
    services.blueman-applet.enable = true;
    services.network-manager-applet.enable = true;

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

    ### Rofi (program launcher)!
    programs.rofi = {
      enable = true;

      theme = lib.mkForce "fancy"; # override the stylix theme.
      # font = "JetBrains Mono Nerd Font 16";
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

    ### compositor! (picom)
    services.picom = {
      enable = true;
      backend = "glx";
      # not that fade is actually enabled...
      fadeExclude = [ "class_g = 'xsecurelock'" ];
    };

    ### the screen locker!
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
          # the addition of the PATH variable is so the screensaver can be loaded...
          # TODO: figure out how to remove or narrow the scope of this path addition
          xsecurelock-base = "PATH=\"/run/current-system/sw/bin\" ${pkgs.xsecurelock}/bin/xsecurelock";
          # to use the htpasswd, need to generate a file, `( umask 077; htpasswd -cB ~/.xsecurelock.pw "$USER" )`, see https://github.com/google/xsecurelock#authentication-protocol-modules
          xsecurelock-htpasswd = "XSECURELOCK_AUTHPROTO=authproto_htpasswd " + xsecurelock-base;
          # need to install xsecurelock outside of nixos if using this.
          xsecurelock-local = "/usr/local/bin/xsecurelock";
          xsecurelock = if config.archerd.useLocalScreenLocker then xsecurelock-local else xsecurelock-base;
          pctl = "${pkgs.playerctl}/bin/playerctl";
          amixer = "${pkgs.alsa-utils}/bin/amixer";
        in
        ''
          /usr/bin/env XSECURELOCK_SAVER=saver_xscreensaver XSECURELOCK_AUTH_TIMEOUT=10 \
                  XSECURELOCK_KEY_XF86AudioPlay_COMMAND="${pctl} -p playerctld play-pause" \
                  XSECURELOCK_KEY_XF86AudioPrev_COMMAND="${pctl} -p playerctld previous" \
                  XSECURELOCK_KEY_XF86AudioNext_COMMAND="${pctl} -p playerctld next" \
                  XSECURELOCK_KEY_XF86AudioStop_COMMAND="${pctl} -p playerctld stop" \
                  XSECURELOCK_KEY_XF86AudioMute_COMMAND="${amixer} set Master toggle" \
                  XSECURELOCK_KEY_XF86AudioLowerVolume_COMMAND="${amixer} set Master 2%-" \
                  XSECURELOCK_KEY_XF86AudioRaiseVolume_COMMAND="${amixer} set Master 2%+" \
                  XSECURELOCK_PASSWORD_PROMPT="time" XSECURELOCK_SHOW_DATETIME=1 \
                  XSECURELOCK_DATETIME_FORMAT="(%%a) %%F T %%R:%%S%%z (%%Z)" ''
        + xsecurelock;
    };
    warnings =
      if config.archerd.useLocalScreenLocker then
        [ "Make sure to install xsecurelock locally!" ]
      else
        [ ];

    ### dunst (notification daemon)
    services.dunst = {
      enable = true;
      iconTheme.package = pkgs.gnome-icon-theme;
      iconTheme.name = "gnome";
      iconTheme.size = "32x32";
      # so many settings!
      settings = {
        global = {
          ### Display ###

          # Which monitor should the notifications be displayed on.
          monitor = 0;

          # Display notification on focused monitor.  Possible modes are:
          #   mouse: follow mouse pointer
          #   keyboard: follow window with keyboard focus
          #   none: don't follow anything
          #
          # "keyboard" needs a window manager that exports the
          # _NET_ACTIVE_WINDOW property.
          # This should be the case for almost all modern window managers.
          #
          # If this option is set to mouse or keyboard, the monitor option
          # will be ignored.
          follow = "mouse";

          # The geometry of the window:
          #   [{width}]x{height}[+/-{x}+/-{y}]
          # The geometry of the message window.
          # The height is measured in number of notifications everything else
          # in pixels.  If the width is omitted but the height is given
          # ("-geometry x2"), the message window expands over the whole screen
          # (dmenu-like).  If width is 0, the window expands to the longest
          # message displayed.  A positive x is measured from the left, a
          # negative from the right side of the screen.  Y is measured from
          # the top and down respectively.
          # The width can be negative.  In this case the actual width is the
          # screen width minus the width defined in within the geometry option.
          geometry = "500x10-30+60";

          # Turn on the progess bar
          progress_bar = true;

          # Set the progress bar height. This includes the frame, so make sure
          # it's at least twice as big as the frame width.
          progress_bar_height = 10;

          # Set the frame width of the progress bar
          progress_bar_frame_width = 2;

          # Set the minimum width for the progress bar
          progress_bar_min_width = 150;

          # Set the maximum width for the progress bar
          progress_bar_max_width = 600;

          # Show how many messages are currently hidden (because of geometry).
          indicate_hidden = "yes";

          # Shrink window if it's smaller than the width.  Will be ignored if
          # width is 0.
          shrink = "no";

          # The transparency of the window.  Range: [0; 100].
          # This option will only work if a compositing window manager is
          # present (e.g. xcompmgr, compiz, etc.).
          transparency = 0;

          # The height of the entire notification.  If the height is smaller
          # than the font height and padding combined, it will be raised
          # to the font height and padding.
          notification_height = 0;

          # Draw a line of "separator_height" pixel height between two
          # notifications.
          # Set to 0 to disable.
          separator_height = 2;

          # Padding between text and separator.
          padding = 8;

          # Horizontal padding.
          horizontal_padding = 8;

          # Padding between text and icon.
          text_icon_padding = 0;

          # Defines width in pixels of frame around the notification window.
          # Set to 0 to disable.
          frame_width = 3;

          # Defines color of the frame around the notification window.
          frame_color = "#aaaaaa";

          # Define a color for the separator.
          # possible values are:
          #  * auto: dunst tries to find a color fitting to the background;
          #  * foreground: use the same color as the foreground;
          #  * frame: use the same color as the frame;
          #  * anything else will be interpreted as a X color.
          # separator_color = "frame"; # stylix defined

          # Sort messages by urgency.
          sort = "yes";

          # Don't remove messages, if the user is idle (no mouse or keyboard input)
          # for longer than idle_threshold seconds.
          # Set to 0 to disable.
          # A client can set the 'transient' hint to bypass this. See the rules
          # section for how to disable this if necessary
          idle_threshold = 60;

          ### Text ###

          # font = "JetBrains Mono 13"; # stylix defined

          # The spacing between lines.  If the height is smaller than the
          # font height, it will get raised to the font height.
          line_height = "1.2";

          # Possible values are:
          # full: Allow a small subset of html markup in notifications:
          #        <b>bold</b>
          #        <i>italic</i>
          #        <s>strikethrough</s>
          #        <u>underline</u>
          #
          #        For a complete reference see
          #        <https://developer.gnome.org/pango/stable/pango-Markup.html>.
          #
          # strip: This setting is provided for compatibility with some broken
          #        clients that send markup even though it's not enabled on the
          #        server. Dunst will try to strip the markup but the parsing is
          #        simplistic so using this option outside of matching rules for
          #        specific applications *IS GREATLY DISCOURAGED*.
          #
          # no:    Disable markup parsing, incoming notifications will be treated as
          #        plain text. Dunst will not advertise that it has the body-markup
          #        capability if this is set as a global setting.
          #
          # It's important to note that markup inside the format option will be parsed
          # regardless of what this is set to.
          markup = "full";

          # The format of the message.  Possible variables are:
          #   %a  appname
          #   %s  summary
          #   %b  body
          #   %i  iconname (including its path)
          #   %I  iconname (without its path)
          #   %p  progress value if set ([  0%] to [100%]) or nothing
          #   %n  progress value if set without any extra characters
          #   %%  Literal %
          # Markup is allowed
          format = "<b>%s</b> %p\n%b";

          # Alignment of message text.
          # Possible values are "left", "center" and "right".
          alignment = "left";

          # Vertical alignment of message text and icon.
          # Possible values are "top", "center" and "bottom".
          vertical_alignment = "center";

          # Show age of message if message is older than show_age_threshold
          # seconds.
          # Set to -1 to disable.
          show_age_threshold = 120;

          # Split notifications into multiple lines if they don't fit into
          # geometry.
          word_wrap = "yes";

          # When word_wrap is set to no, specify where to make an ellipsis in long lines.
          # Possible values are "start", "middle" and "end".
          ellipsize = "middle";

          # Ignore newlines '\n' in notifications.
          ignore_newline = "no";

          # Stack together notifications with the same content
          stack_duplicates = true;

          # Hide the count of stacked notifications with the same content
          hide_duplicate_count = false;

          # Display indicators for URLs (U) and actions (A).
          show_indicators = "yes";

          ### Icons ###

          # Align icons left/right/off
          icon_position = "left";

          # Scale small icons up to this size, set to 0 to disable. Helpful
          # for e.g. small files or high-dpi screens. In case of conflict,
          # max_icon_size takes precedence over this.
          min_icon_size = 24;

          # Scale larger icons down to this size, set to 0 to disable
          max_icon_size = 64;

          # Paths to default icons.
          # see the iconTheme set above #
          # icon_path = "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";

          ### History ###

          # Should a notification popped up from history be sticky or timeout
          # as if it would normally do.
          sticky_history = "yes";

          # Maximum amount of notifications kept in history
          history_length = 40;

          ### Misc/Advanced ###

          # dmenu path.
          dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst";

          # Browser for opening urls in context menu.
          browser = "${pkgs.google-chrome}/bin/google-chrome-stable";

          # Always run rule-defined scripts, even if the notification is suppressed
          always_run_script = true;

          # Define the title of the windows spawned by dunst
          title = "Dunst";

          # Define the class of the windows spawned by dunst
          class = "Dunst";

          # Print a notification on startup.
          # This is mainly for error detection, since dbus (re-)starts dunst
          # automatically after a crash.
          startup_notification = true;

          # Manage dunst's desire for talking
          # Can be one of the following values:
          #  crit: Critical features. Dunst aborts
          #  warn: Only non-fatal warnings
          #  mesg: Important Messages
          #  info: all unimportant stuff
          # debug: all less than unimportant stuff
          verbosity = "mesg";

          # Define the corner radius of the notification window
          # in pixel size. If the radius is 0, you have no rounded
          # corners.
          # The radius will be automatically lowered if it exceeds half of the
          # notification height to avoid clipping text and/or icons.
          corner_radius = 12;

          # Ignore the dbus closeNotification message.
          # Useful to enforce the timeout set by dunst configuration. Without this
          # parameter, an application may close the notification sent before the
          # user defined timeout.
          ignore_dbusclose = false;

          ### Wayland ###
          # These settings are Wayland-specific. They have no effect when using X11

          # Uncomment this if you want to let notications appear under fullscreen
          # applications (default: overlay)
          # layer = "top";

          # Set this to true to use X11 output on Wayland.
          force_xwayland = false;

          ### Legacy

          # Use the Xinerama extension instead of RandR for multi-monitor support.
          # This setting is provided for compatibility with older nVidia drivers that
          # do not support RandR and using it on systems that support RandR is highly
          # discouraged.
          #
          # By enabling this setting dunst will not be able to detect when a monitor
          # is connected or disconnected which might break follow mode if the screen
          # layout changes.
          force_xinerama = false;

          ### mouse

          # Defines list of actions for each mouse event
          # Possible values are:
          # * none: Don't do anything.
          # * do_action: If the notification has exactly one action, or one is marked as default,
          #              invoke it. If there are multiple and no default, open the context menu.
          # * close_current: Close current notification.
          # * close_all: Close all notifications.
          # These values can be strung together for each mouse event, and
          # will be executed in sequence.
          mouse_left_click = "do_action";
          mouse_middle_click = "close_all";
          mouse_right_click = "close_current";
        };

        experimental.per_monitor_dpi = false;
        urgency_low = {
          # IMPORTANT: colors have to be defined in quotation marks.
          # Otherwise the "#" and following would be interpreted as a comment.
          # background = "#222222"; # stylix defined
          # foreground = "#888888"; # stylix defined
          timeout = 15;
          # Icon for notifications with low urgency, uncomment to enable
          #icon = /path/to/icon
        };

        urgency_normal = {
          # background = "#285577"; # stylix defined
          # foreground = "#ffffff"; # stylix defined
          timeout = 30;
          # Icon for notifications with normal urgency, uncomment to enable
          #icon = /path/to/icon
        };

        urgency_critical = {
          # background = "#900000"; # stylix defined
          # foreground = "#ffffff"; # stylix defined
          # frame_color = "#ff0000"; # stylix defined
          timeout = 0; # don't time out.
          # Icon for notifications with critical urgency, uncomment to enable
          #icon = /path/to/icon
          # Play an alert sound ~(nonfunctional)~
          script = "/home/archerd/.dotfiles/scripts/play-alert.sh";
        };

        play-sound = {
          msg_urgency = "critical";
          # TODO: update play-alert script
          script = "/home/archerd/.dotfiles/scripts/play-alert.sh";
        };

        xmonad = {
          appname = "xmonad";
          # background = "#1d7b0a";
          background = "#33572c";
        };
      };
    };
    # alternative is deadd-notification-center
  };
}

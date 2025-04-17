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
    so: xmonad, xmobar, trayer (and contents), screen locker, rofi, ...
    maybe others things, but not sure yet...
  */

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

  ### XMonad & XMobar!
  xsession = {
    enable = true;
    numlock.enable = true;
    # could also use multiline string ('' '') to put contents of the script here.
    # TODO: investigate why some programs aren't launching (notably, trayer).
    initExtra = builtins.readFile ../../xlogin_script;
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
  home.packages = [
    # for xmobar volume?
    pkgs.alsa-utils
  ];


  ### The tray stuff!
  services.trayer = {
    enable = false; # TODO: set this service up, and remove from ../tray.sh
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

  ### Rofi (program launcher)!
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

    # TODO: have this select the correct xsecure configuration based on a config switch somewhere...
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
}

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./windows-drives.nix
    ];

  # Bootloader.
  boot.loader = {
    #systemd-boot.enable = true;
    #systemd-boot.extraEntries = { };
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
    grub = {
      devices = [ "nodev" ];
      efiSupport = true;
      enable = true;
      useOSProber = true; # allows automatic detection of other OS's for booting
    };
    timeout = 10;
  };
  

  # to have the system clock consistent between windows and nixos
  #time.hardwareClockInLocalTime = true;
  
  # Enable networking
  networking.networkmanager.enable = true;  
  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "America/Denver";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.system-config-printer.enable = true;
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip pkgs.gutenprint ];

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };
  ## Enable sound (old version).
  ## sound.enable = true;
  #hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.archerd = {
    isNormalUser = true;
    description = "David Flores";
    extraGroups = [ "networkmanager" "wheel" "adbusers" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCiHn1BtnhzMMIRPDwLRprke/IUARTOyLMwrdCtRdOCGfAmsL/tY92vkMDLIHI8ouCUgIBAyfHmmXcvraXPBKIr7eiwpzxReSJ0xX4tHI6xoLjzXtTglBGIzeGPBDan5IE0KJofmZIGvlnIKz9v9E3f6zP7/185k1l664EAhFjjJeAIhOWvU/8kutET5/Bkc9cRdC6131l+vxj4K+ZAm3W4i9SJd65yvqTZkfI82s6ZHvy7rHeUwyXRzpw4T+hJe9agUJm9bXIPdSJVWYCbb/QFH7PenCT5RgfHkkzMxUlXeoy+Tak+a22eKbJzs907Wga0kx5nZK3viFjN4N24EX8ZmZYS9q+/Rl5V4f6vlhsU4M6XQnEUE4L6UK9+fFFbvQkysTeuAFSLzCTfj6AmnnjdiysSysfKH4+/0o99P+XTU/ToEAgyZEgu2qHIRcD9EPDfjTxCFmH+f33Mo67Ekiix/9uGDknypCSDj3xntrQ4RsQtXbLkUbYdd2WQRIaNukPM0zTWyA2K+ceqktc1yJF6OfvK4sl4BYe5HxAbDQJ/cpFFrvlkcR0FA9tkQ/VCegL0/cSpnn9aUVjM5wvq3WdK20u83ZFNihTyHRyJmcyR/SXkSu2q3LBDpB5U588jxob+rvCXwv0tCRwssA2GcSgPkPOvPMOUB0ueGDRRWulJjw== dflores0818@gmail.com"
    ];
    packages = with pkgs; [
      firefox
    ];
  };

  users.users.testing = {
    isNormalUser = true;
    description = "For Testing Only, may be corrupted/poisoned";
  };

  environment.sessionVariables = {
    # helps java gui applications behave better
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  environment.variables = {
    EDITOR = "nvim";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # terminal emulator
    kitty
    starship
    bash

    # cli applications/utilities
    git
    wget
    bat
    lsd
    scrcpy
    watson # time tracking
    bashmount # tool to manage mounted media (like usb drives)
    # udiskie # alternative to bashmount which should be able to automount usb drives and display tray icon.
    nixos-option # check current nixos configuration values
    texlive.combined.scheme-full
    neofetch
    poppler_utils # pdf utilities
    qmk
    unzip
    pandoc
    abduco # tmux 'alternative'

    # tui applications
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    neovim
    neovide
    cmus
    ranger
    btop

    # gui applications
    google-chrome
    zoom-us
    slack
    evince
    yubioath-flutter
    unison
    gsimplecal
    libreoffice
    discord
    vlc
    jetbrains.idea-ultimate
    burpsuite
    neovide

    # configuration/window manager/desktop environment stuff
    xmobar
    xdotool
    trayer
    xscreensaver
    xsecurelock
    xss-lock
    gnome.nautilus
    gnome.gnome-disk-utility
    gnome.gnome-screenshot
    dmenu
    rofi
    dunst
    logiops
    blueman
    gxmessage
    xfce.xfce4-power-manager
    system-config-printer
    numlockx
    find-cursor
    xclip
    ##visual
    redshift
    lxappearance
    xorg.xsetroot
    autorandr
    arandr
    feh
    ##audio
    playerctl
    pasystray
    paprefs
    pavucontrol

    # languages
    ## haskell configuration
    (haskellPackages.ghcWithPackages (self : [
      self.ghc
      self.xmobar # this line is needed to rebuild xmobar?
      # these are so the haskell language server can work on my xmonad config
      self.xmonad
      self.xmonad-contrib
      self.xmonad-extras
    ]))

    haskellPackages.haskell-language-server
    haskellPackages.hoogle
    haskellPackages.xmobar
    cabal-install
    stack

    ## rust
    cargo
    rust-analyzer

    ## lua
    lua
    lua-language-server

    ## ocaml
    #ocaml

    ## python
    (python3.withPackages (self: [
    	self.ipython
	self.angr
    ]))

    ## scala
    scala
    sbt

    ## java
    #adoptopenjdk-bin
  ];

  # rofi theme path consistency
  environment.etc."rofi/themes".source = "${pkgs.rofi}/share/rofi/themes";

  # my fonts
  fonts.fonts = with pkgs; [
    jetbrains-mono
    (nerdfonts.override { fonts = [ "3270" ]; })
  ];

  #programs.neovim.enable = true;
  #programs.neovim.defaultEditor = true;
  # programs.vim.defaultEditor = true;
  programs.nm-applet.enable = true;
  programs.nm-applet.indicator = true;
  programs.adb.enable = true;
  programs.kdeconnect.enable = true;
  programs.java.enable = true;

  programs.xss-lock = {
    enable = false; # this service is not working as expected, the log says "xsecurelock: No saver selected. Giving up." and the background is a black screen (as opposed to the xmatrix hack from xscreensaver) (still locks though...)
    extraOptions = [
        "--transfer-sleep-lock"
        "-n" "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
        ];
    lockerCommand = "env HOME=\"/home/archerd\" XSECURELOCK_SAVER=saver_xscreensaver XSECURELOCK_PASSWORD_PROMPT=time ${pkgs.xsecurelock}/bin/xsecurelock";
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "archerd" ];

  # enabling experimental features
  # (nix-command enables use of nix to run different things instead many nix-* commands)
  # (flakes enables flakes, which I don't really understand...)
  nix.settings.experimental-features = [ "nix-command" /*"flakes"*/ ];

  # add the systemd service for logiops (logitech mouse support)
  systemd.packages = [ pkgs.logiops ]; # this provides the systemd unit somewhere
  systemd.services.logid = {
    wantedBy = [ "multi-user.target" ]; # consider changing to graphical.target...
  }; # this causes the systemd unit to be enabled/run somehow

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # qmk udev rules
  #services.udev.packages = [ pkgs.qmk-udev-rules ];
  hardware.keyboard.qmk.enable = true; # from nixos.wiki...

  # enable pcscd (for yubioath-flutter)
  services.pcscd.enable = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # X server:
  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.dbus
        haskellPackages.List
        haskellPackages.monad-logger
        haskellPackages.xmonad
	haskellPackages.xmobar
      ];
    };
  };

  # enable picom as the compositor
  services.picom = {
    enable = true;
    backend = "glx";
    fadeExclude = [ "class_g = 'xsecurelock'" ];
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # for chromecast discoverability
  #networking.firewall.allowedUDPPortRanges = [ { from = 32768; to = 60999; } ];
  #networking.firewall = {
  #  allowedUDPPorts = [ 5353 ]; # For device discovery
  #  allowedUDPPortRanges = [{ from = 32768; to = 61000; }];   # For Streaming
  #  allowedTCPPorts = [ 8010 ];  # For gnomecast server
  #};
  services.avahi.enable = true;


  
  # allow autoupgrade (but don't reboot automatically)
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
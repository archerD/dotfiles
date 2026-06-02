{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [
    # TODO: which of these ports needs to be opened?
    # for music-assistant?
    #8097
    #8098
    # for matter-server
    #5580
  ];
  archerd.proxy.virtualHosts = {
    "Music Assistant" = {
      host.ts_subdomain = "mass";
      proxy_to.local_port = 8095;
      abbr = "MA";
    };
    "Home Assistant" = {
      host.ts_subdomain = "hass";
      proxy_to.local_port = 8123;
      abbr = "HA";
    };
  };
  services.music-assistant = {
    enable = true;
    providers = [
      "builtin"
      "chromecast"
      "filesystem_local"
      "filesystem_smb"
      "hass"
      "hass_players"
      "radiobrowser"
      # "template_player_provider"
      "ytmusic"
    ];
    # 2025-04-15: type of provides is list of (one of "airplay", "apple_music", "builtin", "chromecast", "deezer", "dlna", "fanarttv", "filesystem_local", "filesystem_smb", "fully_kiosk", "hass", "hass_players", "jellyfin", "musicbrainz", "opensubsonic", "plex", "qobuz", "radiobrowser", "slimproto", "snapcast", "sonos", "soundcloud", "spotify", "template_player_provider", "test", "theaudiodb", "tidal", "tunein", "ugp", "ytmusic")
  };
  services.home-assistant = {
    enable = true;
    extraComponents = [
      "esphome"
      "nws"
      "radio_browser"
      "hue"
      "cast"
      "google_translate"
      "otp"
      "sun"
      "light"
      "person"
      "device_tracker"
      "device_sun_light_trigger"
      "androidtv"
      "androidtv_remote"
      "cast"
      "device_sun_light_trigger"
      "roku"
      "google_assistant"
      "ipp"
      "tailscale"
      "ibeacon" # to silence errors...
      # new integrations
      "matter"
      "govee_ble"
      "homekit"
      "homekit_controller"
      "aranet"
      "xiaomi_ble"
      "wiz"
      "music_assistant"
      "roomba"
      "google_wifi"
      "upnp"
      "nest" # missing a grpc module...
    ];
    extraPackages =
      py3ps: with py3ps; [
        # pyqrcode # for 2fa
        # spotipy # (spotify api) for ?
        # for faster something... (aio http?)
        zlib-ng
        isal
      ];
    openFirewall = true;
    configWritable = false;
    config = {
      default_config = { };
      # automation = "!include automations.yaml";
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
      };
      homeassistant = {
        auth_mfa_modules = {
          type = "totp";
        };
      };
    };
  };
  # run the matter server for home assistant to use
  services.matter-server = {
    enable = true;
  };
}

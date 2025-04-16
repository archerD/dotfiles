{ config, pkgs, ... }:
{
  networking.firewall.allowedTCPPorts = [
    8097 8098 # for music-assistant?
    5580 # for matter-server
  ];
  services.music-assistant = {
    enable = true;
    providers = [ "builtin" "chromecast" "filesystem_local" "filesystem_smb" "hass" "hass_players" "radiobrowser" "template_player_provider" "ytmusic" ];
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
      # "music_assistant" # currently only in unstable.
    ];
    extraPackages = py3ps: with py3ps; [
            # pyqrcode # for 2fa
            # spotipy # (spotify api) for ?
        ];
    openFirewall = true;
    configWritable = false;
    config = {
      default_config = { };
      automation = "!include automations.yaml";
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" ];
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
  # TODO: have a service to automatically run something like `sudo tailscale funnel --https=443 --set-path=/ "http://127.0.0.1:8123"`, or serve instead of funnel (serve is only accessible on the tailnet, funnel is publically availible).
  # This makes home assistant available at the url nixos-desktop.tail80def.ts.net/
}

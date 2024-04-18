{ config, pkgs, ... }:
{
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
    ];
    extraPackages = py3ps: with py3ps; [
            pyqrcode # for 2fa
            spotipy # (spotify api) for ?
        ];
    openFirewall = true;
    configWritable = false;
    config = {
      default_config = { };
      automation = "!include automations.yaml";
      homeassistant = {
        auth_mfa_modules = {
          type = "totp";
        };
      };
    };
  };
}

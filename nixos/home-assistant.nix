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
      "tailscale"
      "ibeacon" # to silence errors...
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
  # TODO: have a service to automatically run something like `sudo tailscale funnel --https=443 --set-path=/ "http://127.0.0.1:8123"`, or serve instead of funnel (serve is only accessible on the tailnet, funnel is publically availible).
  # This makes home assistant available at the url nixos-desktop.tail80def.ts.net/
}

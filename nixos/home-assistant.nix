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
        ];
        extraPackages = py3ps: with py3ps; [ pyqrcode ]; # for 2fa
        openFirewall = true;
        configWritable = false;
        config = {
            default_config = {};
            automation = "!include automations.yaml";
            homeassistant = {
                auth_mfa_modules = { type = "totp"; };
            };
        };
    };

}


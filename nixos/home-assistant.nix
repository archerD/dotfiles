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
        ];
        openFirewall = true;
        configWritable = false;
        config = {
            default_config = {};
            automation = "!include automations.yaml";
        };
    };

}


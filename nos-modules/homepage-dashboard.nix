{ config, inputs, pkgs, ...}:
{
  services.homepage-dashboard = {
    enable = true;
    # listenPort = 8082;

    # homepage expects the background to be in /app/public/images...
    # TODO: this forces homepage to be rebuilt everytime, see if we can optimize this...
    package = pkgs.homepage-dashboard.overrideAttrs (_: {
      postInstall = ''
        mkdir -p $out/share/homepage/public/images
        ln -s ${inputs.self}/images/* $out/share/homepage/public/images
        ln -s ${config.stylix.image} $out/share/homepage/public/stylix.png
      '';
    });

    openFirewall = false;
    environmentFile = "";
    settings = {
      title = "DEF home";
      description = "Testing description";
      # set image above
      background = "/images/nixos-dark-tiling.png";
      # background = "/stylix.png"
      theme = "dark";
      color = "stone";
      # color = "green";
      # color = "emerald";
      headerStyle = "boxedWidgets";
      # headerStyle = "underlined";
      language = "en";
      # target options are _blank (new tab), _self (same tab), _top (new window)
      target = "_self";
      quicklaunch = {
        searchDescriptions = false;
        provider = "google";
      };
      # basic = "dot";

      # layouts...
      # layout = {
      #   Developer = {
      #     style = "row";
      #     columns = 4;
      #   };
      # };
    };
    bookmarks = [
      {
        Developer = [
          {
            GitHub = [
              {
                abbr = "GH";
                href = "https://github.com/";
              }
            ];
          }
          {
            "NixOS Package Search" = [
              {
                abbr = "NP";
                href = "https://search.nixos.org/packages?";
              }
            ];
          }
          {
            "NixOS Option Search" = [
              {
                abbr = "NO";
                href = "https://search.nixos.org/options?";
              }
            ];
          }
          {
            "Home Manager Options" = [
              {
                abbr = "HM";
                href = "https://nix-community.github.io/home-manager/options.xhtml";
              }
            ];
          }
        ];
      }
      {
        Utilities = [
          {
            Home-Assistant = [
              {
                abbr = "HA";
                href = "http://nixos-desktop.tail80def.ts.net:8123";
              }
            ];
          }
          {
            Music-Assistant = [
              {
                abbr = "MA";
                href = "http://nixos-desktop.tail80def.ts.net:8095";
              }
            ];
          }
          {
            Tailscale = [
              {
                abbr = "TS";
                href = "https://login.tailscale.com/admin/machines";
              }
            ];
          }
          {
            "Hue Control" = [
              {
                abbr = "HC";
                href = "https://jakobjfl.github.io/Hue-Browser-Controller/";
              }
            ];
          }
        ];
      }
      {
        Entertainment = [
          {
            YouTube = [
              {
                abbr = "YT";
                href = "https://youtube.com/";
              }
            ];
          }
          {
            "Disney+" = [
              {
                abbr = "DP";
                href = "https://disneyplus.com/";
              }
            ];
          }
          {
            Hulu = [
              {
                abbr = "HU";
                href = "https://hulu.com/";
              }
            ];
          }
          {
            "Prime Video" = [
              {
                abbr = "PV";
                href = "https://amazon.com/gp/video/storefront";
              }
            ];
          }
        ];
      }
    ];
    services = [];
    # widgets are at the top
    widgets = [
      {
        resources = {
          uptime = true;
          cpu = true;
          cputemp = true;
          memory = true;
          disk = "/";
          diskUnits = "bbytes";
          network = true; # not reporting any data...
          expanded = true;
        };
      }
      # {
      #   logo = {};
      # }
      {
        datetime = {
          text_size = "2x1";
          locale = "en-SE";
          format = {
            dateStyle = "short";
            timeStyle = "medium";
            # hourCycle = "h24";
          };
        };
      }
      {
        search = {
          provider = "google";
          target = "_blank";
        };
      }
      {
        openmeteo = {
          cache = 5;
          units = "imperial";
          label = "Boulder";
          latitude = 40.015;
          longitude = -105.271;
        };
      }
    ];
    kubernetes = {};
    docker = {};
    customCSS = "";
    customJS = "";
  };
}

{ lib
, config
, pkgs
, ...
}:
let
  hostname = config.archerd.server.host;
  pubDomain = "";
  tsDomain = "";
  subdomainRE = "([[:alnum:]]+)([.][[:alnum:]]+)*";
  matcher_type = with lib.types; attrTag {
    ts_subdomain = lib.mkOption {
      description = "The subdomain this lives at under the tailscale subdomain.";
      type = strMatching subdomainRE;
    };
    pub_subdomain = lib.mkOption {
      description = "The subdomain this lives at under the public subdomain.";
      type = strMatching subdomainRE;
    };
    ts_port = lib.mkOption {
      description = "A port in the tailnet domain";
      type = port;
    };
    host_port = lib.mkOption {
      description = "A port of the hostname, insecure by default.";
      type = port;
    };
    local_port = lib.mkOption {
      description = "A port of localhost.";
      type = port;
    };
    # direct = lib.mkOption {
    #   description = "The exact matcher/url to use";
    #   type = str;
    # };
  };
  add_url_attr = matcher: 
      if matcher ? ts_subdomain then
        matcher // { url = matcher.ts_subdomain + ".ts.${pubDomain}"; porkbun = true; }
      else if matcher ? pub_subdomain then
        matcher // { url = matcher.pub_subdomain + ".${pubDomain}"; porkbun = true; }
      else if matcher ? ts_port then
        matcher // { url = "${tsDomain}:${builtins.toString matcher.ts_port}"; }
      else if matcher ? host_port then
        matcher // { url = "${hostname}:${builtins.toString matcher.host_port}"; insecure = true; }
      else if matcher ? local_port then
        matcher // { url = "localhost:${builtins.toString matcher.local_port}"; }
        # matcher // { url = "${config.archerd.server.ip_addr}:${builtins.toString matcher.local_port}"; }
      else if matcher ? direct then
        matcher // { url = matcher.direct; }
      else throw "Unexpected error: no matching url builder for this matcher type!";
  extract_url = matcher: matcher.url;
  matcher_option = description: lib.mkOption {
    description = description + " (read the resulting url at the url attribute of this option)";
    type = matcher_type;
    apply = add_url_attr;
  };
in { # This is a reverse proxy, but meh.
  options = {
    archerd.proxy.enable = lib.mkEnableOption "proxy support with caddy (porkbun.com for domains)";
    # NOTE: additional things needed for this to work: an A record exists for the domains, and port forwarding set up on router.
    archerd.proxy.public_ip = lib.mkEnableOption "support for a public ip adress via porkbun";
    archerd.proxy.porkbunEnvironmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = "/home/archerd/.dotfiles/porkbun_api_keys.env";
    };
    archerd.proxy.enable_test_subdomains = lib.mkEnableOption "a couple of testing subdomains.";

    archerd.proxy.virtualHosts = lib.mkOption {
      description = "The hosts, managed by caddy";
      default = { };
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          host = matcher_option "The host matcher to proxy";
          extra_hosts = lib.mkOption {
            description = "Other server aliases to add.";
            type = lib.types.listOf matcher_type;
            default = [];
            apply = map add_url_attr;
          };
          proxy_to = matcher_option "Where to send the request to.";
          abbr = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            description = "Abbreviation for homepage dashboard stuff";
            default = null;
          };
          # extraConfig = lib.mkOption {
          #   type = lib.types.lines;
          #   default = "";
          #   description = "additional things to be added to the caddy config...";
          # };
        };
      });
    };
  };

  config = lib.mkIf config.archerd.proxy.enable {
    services.tailscale.permitCertUid = "caddy"; # allows proper fetching of keys/certificates for tsDomain addresses.
    networking.firewall.allowedTCPPorts = lib.optionals config.archerd.proxy.public_ip [ 443 ];
    systemd.services.ddclient.serviceConfig.EnvironmentFile = config.archerd.proxy.porkbunEnvironmentFile;
    services.ddclient = {
      enable = config.archerd.proxy.public_ip;
      protocol = "porkbun";
      domains = lib.map extract_url (lib.filter (matcher: matcher ? pub_subdomain)
          (lib.attrsets.mapAttrsToList (_n: vhost: vhost.host) config.archerd.proxy.virtualHosts));
      extraConfig = ''
        apikey_env=PORKBUN_API_KEY
        secretapikey_env=PORKBUN_API_SECRET_KEY
        '';
    };

    archerd.proxy.virtualHosts = lib.mkIf config.archerd.proxy.enable_test_subdomains {
      "Public Test" = {
        host.pub_subdomain = "public";
        proxy_to.local_port = 44300;
      };
      "Tailscale Test" = {
        host.ts_subdomain = "test";
        proxy_to.local_port = 8000;
      };
    };

    services.caddy = {
      enable = true;
      # Adds support for using porkbun certificates.
      package = pkgs.caddy.withPlugins {
        plugins = [ "github.com/caddy-dns/porkbun@v0.3.1" ];
        hash = "sha256-BKUsUoBE1IjnD9Xu8kTVkbRqqk2qvNtFDD/pvVkfRmI=";
      };
      environmentFile = config.archerd.proxy.porkbunEnvironmentFile;
      extraConfig = ''
        (porkbun) {
          tls {
            dns porkbun {
              api_key {$PORKBUN_API_KEY}
              api_secret_key {$PORKBUN_API_SECRET_KEY}
            }
          }
        }
        '';
      # compile my virtual hosts into caddy virtual hosts
      virtualHosts = lib.attrsets.mapAttrs
        (name: vhost: {
          hostName = vhost.host.url;
          serverAliases = lib.map extract_url vhost.extra_hosts;
          extraConfig = lib.concatLines
            (
              (lib.optional (vhost.host ? porkbun && vhost.host.porkbun) "import porkbun") ++
              [ 
                # vhost.extraConfig
                "reverse_proxy ${vhost.proxy_to.url}"
              ]
            );
        })
        config.archerd.proxy.virtualHosts;
    };
  };
}

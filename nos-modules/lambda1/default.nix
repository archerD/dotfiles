{lib, ...}:
let bentopdf_domain = "BentoPDF";
in {
    # lambda1 specific configuration.
    imports = [
        ./hardware-configuration.nix
        ./windows-drives.nix
        ../server
    ];

    networking.hosts = {
        # force lambda1 through the tailscale network ip address.
        "127.0.0.2" = lib.mkForce [];
        "tailscale0" = ["lambda1"];
    };

    archerd.proxy.virtualHosts.${bentopdf_domain} = {
        # host.pub_subdomain = "pdf";
        host.ts_subdomain = "pdf";
    };
    services.bentopdf = {
        enable = true;
        domain = bentopdf_domain;
        caddy.enable = true;
    };

    archerd.proxy.enable_test_subdomains = true;

    networking.hostName = "lambda1";

    archerd.secure-boot = false;
}

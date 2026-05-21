{config, pkgs, lib, ...}:
let cfg = config.archerd.ai;
in {
    options = {
        archerd.ai = {
            enable = lib.mkEnableOption "Enable ai things...";
        };
    };

    config = lib.mkIf cfg.enable {
        services.ollama = {
            enable = true;
            package = pkgs.ollama-cuda;
            # syncModels = true;
            loadModels = [
                "qwen3:8b"
                "qwen3-vl:8b"
                # "qwen3-coder:7b"
                "codeqwen"
                "deepseek-coder:6.7b"
                # "gpt-oss:20b"
            ];

            # Make generally available on my network
            openFirewall = false; # not needed for in-tailnet usage
            # host = archerd.server.ip_addr;
            host = "0.0.0.0";
        };

        environment.systemPackages = [
            pkgs.oterm
            pkgs.lsp-ai
        ];
    };
}

{config, pkgs, lib, llm-pkgs, ...}:
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
            syncModels = true;
            loadModels = [
                # "qwen3-vl:8b"
                "qwen3-coder:30b" # somewhat recent, released with qwen-code
                "qwen3.5:9b" # somewhat compact model, somewhat recent.
                "qwen3.6:27b" # 'latest' qwen model
                "codeqwen:7b" # small older model
                # "deepseek-coder-v2:16b"
                # "gpt-oss:20b"
            ];
            environmentVariables = {
                OLLAMA_CONTEXT_LENGTH = "32768"; # 2^15
                OLLAMA_FLASH_ATTENTION = "1";
                OLLAMA_KV_CACHE_TYPE = "q8_0";
            };

            # Make generally available on my network
            openFirewall = false; # not needed for in-tailnet usage
            # host = archerd.server.ip_addr;
            host = "0.0.0.0";
        };

        environment.systemPackages = [
            pkgs.oterm
            pkgs.lsp-ai
            llm-pkgs.qwen-code
        ];
    };
}

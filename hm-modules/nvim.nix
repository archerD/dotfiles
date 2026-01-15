{
  inputs,
  pkgs,
  lib,
  ...
}:
let
  # these functions from https://gist.github.com/nat-418/d76586da7a5d113ab90578ed56069509
  # purpose is to package neovim plugins not in nixpkgs, should I use nix to manage my plugins...
  # repo, branch, commit
  fromGitHubCommit =
    repo: ref: rev:
    pkgs.vimUtils.buildVimPluginFrom2Nix {
      pname = "${lib.strings.sanitizeDerivationName repo}";
      version = ref;
      src = builtins.fetchGit {
        url = "https://github.com/${repo}.git";
        ref = ref;
        rev = rev;
      };
    }; # e.g. (fromGitHubCommit "elihunter173/dirbuf.nvim" "HEAD" "6422c3a651c3788881d01556cb2a90bdff7bf002")
  # name, flake input
  fromFlakeInput =
    pname: src:
    pkgs.vimUtils.buildVimPluginFrom2Nix {
      inherit pname src;
      version = src.rev;
    };
in
# e.g. (fromFlakeInput "dirbuf" inputs.dirbuf), with the following in the inputs to the flake:
# dirbuf = { flake = false; url = github:elihunter173/dirbuf.nvim; }
{
  # don't use stylix theming!
  stylix.targets.neovim.enable = false;

  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraPackages =
      let
        # ripgrep and fd for telescope, gcc for treesitter.
        pluginPkgs = with pkgs; [
          ripgrep
          fd
          gcc
          tree-sitter
          nodejs-slim
        ];
        ghcPkg = pkgs.haskellPackages.ghcWithPackages (self: [
          self.ghc
          self.xmobar # this line is needed to rebuild xmobar?
          # these are so the haskell language server can work on my xmonad config
          self.xmonad
          self.xmonad-contrib
          self.xmonad-extras
        ]);
        # hlsPkg = pkgs.haskell-language-server.override { supportedGhcVersions = ["884"]; } ;
        hlsPkg = pkgs.haskell-language-server;
        lspPkgs = [
          ghcPkg
          hlsPkg
          pkgs.nixd
          pkgs.nixfmt
          # the cpp lsp is better served by having clang-tools in the flake.
          pkgs.clang-tools
          pkgs.gopls
          pkgs.go
          # pkgs.texlab
        ];
      in
      pluginPkgs ++ lspPkgs;

    # plugin management? needs further investigation
    # plugins = with pkgs.vimPlugins; [
    #         nvim-treesitter
    #         # nvim-treesitter.withAllGrammars
    #     ];
    # extraLuaConfig = /* lua */ ''
    #     require("keybindings")
    #     require("settings")
    # '';
  };
}

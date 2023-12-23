{ inputs, pkgs, lib, ...}:
let # these functions from https://gist.github.com/nat-418/d76586da7a5d113ab90578ed56069509
    # purpose is to package neovim plugins not in nixpkgs, should I use nix to manage my plugins...
  # repo, branch, commit
  fromGitHubCommit = repo: ref: rev: pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
      rev = rev;
    };
  }; # e.g. (fromGitHubCommit "elihunter173/dirbuf.nvim" "HEAD" "6422c3a651c3788881d01556cb2a90bdff7bf002")
  # name, flake input
  fromFlakeInput = pname: src: pkgs.vimUtils.buildVimPluginFrom2Nix {
    inherit pname src;
    version = src.rev;
  }; # e.g. (fromFlakeInput "dirbuf" inputs.dirbuf), with the following in the inputs to the flake:
     # dirbuf = { flake = false; url = github:elihunter173/dirbuf.nvim; }
in
{
  programs.neovim = {
    enable = true;
    defaultEditor = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    # ripgrep and fd for telescope, gcc for treesitter.
    extraPackages = with pkgs; [ ripgrep fd gcc ];

    # plugin management? needs further investigation
    plugins = with pkgs.vimPlugins; [
            # nvim-treesitter
            nvim-treesitter.withAllGrammars
            ### vim-plugins
            vim-repeat
            vim-surround
            vim-fugitive
            vim-obsession
            #vim-commentary

            #vim-textobj-fold # not in nixpkgs
            vim-gitgutter

            vim-localvimrc
            undotree
            vimtex # need to add the configuration.
        ];
    extraLuaConfig = /* lua */ ''
        require("keybindings")
        require("settings")
    '';
  };
}

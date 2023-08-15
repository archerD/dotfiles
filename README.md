# My dotfiles
Contains my dot files.
Uses [dotbot](https://github.com/anishathalye/dotbot) as a dot file manager.
The .vim files are added as a subtree so that the vim stuff can be cloned without having to deal with the other stuff.
The main repository for the .vim files is [archerD/.vim](https://github.com/archerD/.vim).

## NixOS
To update nixpkgs, run `nix flake update` and commit the resulting changes to the flake.lock file.
Then run `sudo nixos-rebuild switch` (`sudo nixos-rebuild switch --flake ~/.dotfiles/nixos#nixos-desktop`?).
### Home Manager...
Currently, set up to be run when the system configuration is applied...
Not clear on exactly what it is doing now...

## DotBoT Usage
To install the dotfiles using dotbot on a machine, clone this repository (git clone ...), then run the install script inside it.
This should install the configuration, using the instal.conf.yaml file as a guide.

To update the dotbot version, run git submodule update --remote dotbot, and commit.

## Dealing with the .vim files
Changing the .vim files can be done as before, though it would be wise to make seperate commits for changes to the .vim files and changes to other files.

To sync up with the main repository for the .vim files, the following commands can be used:
* git subtree pull -P vim/ git@github.com:archerD/.vim main
* git subtree push -P vim/ git@github.com:archerD/.vim main


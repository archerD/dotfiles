# My dotfiles
Contains my dot files.
Uses [dotbot](https://github.com/anishathalye/dotbot) as a dot file manager.
The .vim files are added as a subtree so that the vim stuff can be cloned without having to deal with the other stuff.
The main repository for the .vim files is [archerD/.vim](https://github.com/archerD/.vim).

## NixOS
To update nixpkgs, run `nix flake update` and commit the resulting changes to the flake.lock file.
Then run `sudo nixos-rebuild switch` (`sudo nixos-rebuild switch --flake ~/.dotfiles/nixos#nixos-desktop`?).
### Home Manager...
Currently, set up to be run when the system configuration is applied on desktop...
Not clear on exactly what it is doing now...
Can be applied on laptop running `home-manager switch --flake .` from the nixos/ directory.

## Laptop versions
To keep in sync with main, run `git rebase main laptop` from the laptop branch.
To add more changes to the laptop version, `git rebase -i HEAD~3` allows modifying the last 3 commits as appropriate.
A force push will be required after doing either of these operations.
Changes so far:
* size related changes
  * tray.sh
    * changing the height of trayer
  * xmobar.hs
    * changing the font sizes
    * changing the total size of the bar
* distribution based file location changes
  * dunstrc
    * change the location of the rofi executable
  * rofi.rasi
    * change the theme location
  * xss-lock-xsecurelock-daemon.sh
    * use bash for the shebang instead of nix-shell
  * xmonad.hs
    * run the xss-lock-xsecure-daemon.sh file (screen locker is broken...)
  * nixos/home.nix
    * Use the generic linux target
    * lock screen using locally installed xsecurelock
    * add an alias so sudo can be used with packages installed by home-manager
* distribution based autolaunching changes
  * xlogin_script launches picom (nixos does this automatically)

## DotBoT Usage
To install the dotfiles using dotbot on a machine, clone this repository (git clone ...), then run the install script inside it.
This should install the configuration, using the instal.conf.yaml file as a guide.

To update the dotbot version, run git submodule update --remote dotbot, and commit.

## Dealing with the .vim files
Changing the .vim files can be done as before, though it would be wise to make seperate commits for changes to the .vim files and changes to other files.

To sync up with the main repository for the .vim files, the following commands can be used:
* git subtree pull -P vim/ git@github.com:archerD/.vim main
* git subtree push -P vim/ git@github.com:archerD/.vim main


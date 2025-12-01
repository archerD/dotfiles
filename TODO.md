# Other things that would be good to do
## refactoring/cleanup/maintaince...
* move more things from tray.sh into home-manager
* continue refactoring nix modules, including creating a default.nix for home manager, nixos, and system manager
* package scripts with a tool like [resholve](github.com/abathur/resholve), or [writeShellApplication](https://nixos.org/manual/nixpkgs/stable/#trivial-builder-writeShellApplication). See this [discourse thread](https://discourse.nixos.org/t/best-practice-on-handling-executable-paths-in-scripts-when-using-home-manager-on-nixos-and-non-nixos/35994)
* investigate clustergit packaging.
* consider creating a system-manager systemd services/tempfiles rule (like nix-system-graphics does), to link system-manager bin into a place the ubuntu sudo program considers part of the path.
* Update flake inputs to 24.11->25.05
    [x] services.homepage-dashboard.allowedHosts = "localhost:8082,nixos-desktop.tail80def.ts.net:8082"
    [ ] new things to look at nixos modules
        - look at homer as alternative to homepage-dashboard.
    [ ] new things to look at home manager modules
        - onagre launcher
* HIGH PRIORITY: Update neovim LSP configuration! (updating nvim-lspconfig no longer works do to breaking changes).
## fixes
* investigate the xsecurelock path issue and why it will not launch the screensaver if the path is not set
    * another issue: failed parsing of the xscreensaver config file on laptop...
## new features
* more stylix stuff! see (./style.md)
* revisit the cl script (cd && ls) and enable it
* make a script to mv file to file.bak and then cp file.bak to file.
    + optionally, add flag to swap file and file.bak, and flag to just mv file.bak to file
    + optionally, make capable of detecting when file.bak exists and do reasonable thing (error out, add more .bak, try different suffix).
* consider moving away from dotbot to home-manager for installing dotfiles.
* investigate different nvim configuration frameworks.
* investigate running dotbot from home-manager.
* incorporate secret management tool (like agenix or sops-nix)
* integrate a rofi powermenu, and the ability to launch it from xmobar with the mouse.

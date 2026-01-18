# Other things that would be good to do
## refactoring/cleanup/maintaince...
* move more things from tray.sh into home-manager
* continue refactoring nix modules, including creating a default.nix for home manager, nixos, and system manager
* package scripts with a tool like [resholve](github.com/abathur/resholve), or [writeShellApplication](https://nixos.org/manual/nixpkgs/stable/#trivial-builder-writeShellApplication). See this [discourse thread](https://discourse.nixos.org/t/best-practice-on-handling-executable-paths-in-scripts-when-using-home-manager-on-nixos-and-non-nixos/35994)
* investigate clustergit packaging.
* consider creating a system-manager systemd services/tempfiles rule (like nix-system-graphics does), to link system-manager bin into a place the ubuntu sudo program considers part of the path.
* Update flake inputs to 25.05->25.11
    [ ] secureboot support with limine bootloader.
        - actual support seems a bit limited at the moment, revisit later.
    [ ] new things to look at nixos modules
        - argrr for gc root removal.
        - LACT for gpu monitoring/control.
        - pmount for mounting drives without sudo
    [ ] new things to look at home manager modules
        - numbat? calculator/PL with unit support. could replace the rofi calculator...
* update xmobar to differentiate between hosts
    + could use host name, or a symbol (a lambda for lambda1, a laptop or the ubuntu logo, or maybe yoga for x1yoga4, for fractal... maybe one of the benzene chars? an electrical/vector intersection? crossing lanes? a snowflake? nixos+windows logo? turned capital y? ocr inverted fork? an axis variant? triforce?)
## fixes
* investigate the xsecurelock path issue and why it will not launch the screensaver if the path is not set
    * another issue: failed parsing of the xscreensaver config file on laptop...
## new features
* Hosting things:
    + ersatsTV - tv thing of your video library.
    + ente/immich - for photos (and videos?) (google alternative)
    + jellyfin - media library
    + homepage - options include homer (localhost), homepage (localhost:3567), dashy (localhost)
* patch kitty to have more powerline options for the tabbar
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

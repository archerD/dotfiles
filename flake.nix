{
  description = "NixOS flake for archerD";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    stylix.url = "github:danth/stylix/release-24.11";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    stylix.inputs.home-manager.follows = "home-manager";

    # nix index, for comma.
    nix-index-database.url = "github:Mic92/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    # flake checker
    flake-checker.url = "github:DeterminateSystems/flake-checker";
    flake-checker.inputs.nixpkgs.follows = "nixpkgs";

    # clustergit
    clustergit.url = "github:mnagel/clustergit";
    clustergit.flake = false;

    # lanzaboote, for secure boot
    lanzaboote.url = "github:nix-community/lanzaboote/v0.4.2";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";

    # TODO: Add any other flake you might need
    # hardware.url = "github:nixos/nixos-hardware";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      stylix,
      nix-index-database,
      lanzaboote,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      args = {
        # Pass flake inputs to our config
        inherit inputs;
        pkgs-unstable = import nixpkgs-unstable {
          # configure the unstable inputs...
          inherit system;
          # config.allowUnfree = true;
        };
        pkgs-mine = inputs.self.packages.${system};
      };
    in
    {
      # my custom packages not in nixpkgs...
      packages.${system} = import ./custom-packages.nix { inherit inputs system; };
      # an (unused, untested) overlay to add the pkgs to the nixpkgs input...
      # overlays.${system}.additions = final: _prev: import ./custom-packages.nix { pkgs = final; inherit inputs; };

      # formatter!
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-rfc-style;

      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        # the desktop setup...
        NixOS-Desktop = nixpkgs.lib.nixosSystem {
          inherit system;

          specialArgs = args;
          # > Our main nixos configuration file <
          modules = [
            ./nos-modules/configuration.nix
            stylix.nixosModules.stylix
            lanzaboote.nixosModules.lanzaboote
            ./stylix-theme.nix

            /*
              # make home-manager as a module of nixos
              # so that home-manager configuration will be deployed automatically when executing `nixos-rebuild switch`
              home-manager.nixosModules.home-manager
              {
                # home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.verbose = true;

                home-manager.extraSpecialArgs = args; # Pass flake inputs to our config

                home-manager.users.archerd = import ./hm-modules/home.nix;
              }
              {
                # also configure the home manager module!
                home-manager.users.archerd.archerd.targetGenericLinux = true;
                home-manager.users.archerd.archerd.highResolutionScreen = true;
              }
            */
          ];
        };
      };

      #/*
      # Standalone home-manager configuration entrypoint
      # Available through 'home-manager --flake .#your-username@your-hostname'
      homeConfigurations = {
        "archerd@Ubuntu-X1-Yoga-4" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system}; # Home-manager requires 'pkgs' instance
          extraSpecialArgs = args; # Pass flake inputs to our config
          # > Our main home-manager configuration file <
          modules = [
            ./hm-modules/home.nix
            stylix.homeManagerModules.stylix
            ./stylix-theme.nix
            { archerd.targetGenericLinux = true; }
          ];
        };
        "archerd@NixOS-Desktop" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system}; # Home-manager requires 'pkgs' instance
          extraSpecialArgs = args; # Pass flake inputs to our config
          # > Our main home-manager configuration file <
          modules = [
            ./hm-modules/home.nix
            stylix.homeManagerModules.stylix
            ./stylix-theme.nix
            {
              archerd.targetGenericLinux = false;
              archerd.highResolutionScreen = true;
            }
          ];
        };
      };
      #*/
    };
}

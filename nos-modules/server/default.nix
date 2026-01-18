{...}:
{
  imports = [
    ./homepage-dashboard.nix
    ./open-home.nix
    ./postgresql.nix
  ];

  options = {
  };

  config = {
    archerd.homepage-server = "homepage";
  };
}

{...}:
{
  imports = [
    ./homepage-dashboard.nix
    ./open-home.nix
    ./postgresql.nix
    ./video-server.nix
  ];

  options = {
  };

  config = {
    archerd.homepage-server = "homepage";
  };
}

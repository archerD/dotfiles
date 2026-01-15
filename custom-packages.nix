{
  inputs,
  system,
  ...
}:
let
  pkgs = import inputs.nixpkgs {
    inherit system;
    # config.allowUnfree = true;
  };
  pkgs-unstable = import inputs.nixpkgs-unstable {
    inherit system;
    # config.allowUnfree = true;
  };
in
rec {
  #NOTE: packaging is kept more for remembering how to do this then for any usefulness, it's in nixpkgs now.
  zenlog = pkgs.python3Packages.buildPythonPackage rec {
    pname = "zenlog";
    version = "1.1";
    src = pkgs.fetchPypi {
      inherit pname version;
      sha256 = "83460a85fa7249b8007c03681a6a0b575ce6fe044349389d3d3d43f58d4687de";
    };
    doCheck = false;
    pyproject = true;
    build-system = [ pkgs.python3Packages.setuptools ];
    propagatedBuildInputs = with pkgs.python3Packages; [ colorlog ];
  };
  #NOTE: pyradios from https://github.com/NixOS/nixpkgs/blob/nixos-25.11/pkgs/by-name/ra/radio-active/package.nix#L10
  pyradios_1-0-2 = pkgs.python3Packages.pyradios.overrideAttrs (
    finalAttrs: previousAttrs:
    let
      version = "1.0.2";
    in {
      inherit version;

      src = previousAttrs.src.override {
        inherit version;
        hash = "sha256-O30ExmvWu4spwDytFVPWGjR8w3XSTaWd2Z0LGQibq9g=";
      };
    }
  );
  radio-active = pkgs.python3Packages.buildPythonApplication rec {
    pname = "radio-active";
    version = "2.9.1";
    src = pkgs.fetchPypi {
      inherit pname version;
      sha256 = "ABdnGKauuwxMxVHEMIF7VwxTxcriUqs1XRwB1FYcYJY=";
    };
    doCheck = false;
    pyproject = true;
    build-system = [ pkgs.python3Packages.setuptools ];
    propagatedBuildInputs = with pkgs.python3Packages; [
      pkgs.ffmpeg_4-full
      requests
      urllib3
      psutil
      pyradios_1-0-2
      requests-cache
      rich
      pick
      zenlog

      flake8
      twine
      black
    ];
  };
  # TODO: I don't like this way of packaging clustergit, look into alternatives
  clustergit-script =
    (pkgs.writeScriptBin "clustergit" (builtins.readFile "${inputs.clustergit}/clustergit"))
    .overrideAttrs
      (old: {
        buildCommand = "${old.buildCommand}\n patchShebangs $out";
      });
  clustergit = pkgs.symlinkJoin {
    name = "clustergit";
    paths = with pkgs; [
      clustergit-script
      git
      python3
    ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = "wrapProgram $out/bin/clustergit --prefix PATH : $out/bin";
  };
  acpi-backlight-notify = pkgs.stdenv.mkDerivation (finalAttrs: {
    pname = "backlight-notify";
    version = "1.0.0";

    src = pkgs.fetchFromGitHub {
      owner = "ikrivosheev";
      repo = "acpi-backlight-notify";
      rev = "v${finalAttrs.version}";
      hash = "sha256-SIy/GQMpTdub2zq6Ed9L4pyHzhlugkqSLSIJUMyJgmA=";
    };

    nativeBuildInputs = with pkgs; [
      cmake
      extra-cmake-modules
      pkg-config
    ];

    buildInputs = with pkgs; [
      glib
      libnotify
      xorg.libxcb
      xorg.xcbutil
    ];
  });
  mpvScripts = {
    toggle-shuffle = pkgs.mpvScripts.buildLua {
      pname = "mpv-toggle-shuffle";
      version = "0-unstable-2025-09-26";
      src = pkgs.fetchFromGitHub {
        owner = "NaiveInvestigator";
        repo = "toggle-shuffle";
        rev = "ee0be1d3eb9cf4ef0d669adcd83da733c2c53a54";
        hash = "sha256-HDhPSSErRtH97BGi8VZc6Jyb0dGaDySfhS3R4Se1aOs=";
      };

      passthru.updateScript = pkgs.gitUpdater {};
      passthru.scriptName = "toggle-shuffle.js";
    };
  };
}

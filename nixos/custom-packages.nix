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
in rec {
  zenlog = pkgs.python3Packages.buildPythonPackage rec {
    pname = "zenlog";
    version = "1.1";
    src = pkgs.fetchPypi {
      inherit pname version;
      sha256 = "83460a85fa7249b8007c03681a6a0b575ce6fe044349389d3d3d43f58d4687de";
    };
    doCheck = false;
    propagatedBuildInputs = with pkgs.python3Packages; [ colorlog ];
  };
  radio-active = pkgs.python3Packages.buildPythonApplication rec {
    pname = "radio-active";
    version = "2.8.0";
    src = pkgs.fetchPypi {
      inherit pname version;
      sha256 = "7d01ce460cac3b57f421762c8943187ee3bd458e51985d9d15d37381b6fe265c";
    };
    doCheck = false;
    propagatedBuildInputs = with pkgs.python3Packages; [
      pkgs.ffmpeg_4-full
      requests
      urllib3
      psutil
      pyradios
      requests-cache
      rich
      pick
      zenlog

      flake8
      twine
      black
    ];
  };
  # TODO: I don't like this way of packaging clustergitt, look into alternatives
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
}

{pkgs, ...} :
{ # NOTE: PostgreSQL version was at 17 at last upgrade.
  options = {
  };
  config = {
    services.postgresql = {
      enable = true;
      # WARN: allows tailnet access, but may not be the most secure.
      # enableTCPIP = true;
      # authentication = ''
      #   # allow hosts in my tailnet to connect using passwords
      #   host  all   all   .tail80def.ts.net   md5
      #   '';
    };
    services.pgadmin = {
      enable = true;
      initialEmail = "dflores0818@gmail.com";
      # needed for first run of the program...
      initialPasswordFile = "/home/archerd/.dotfiles/mypass";
    };
    environment.systemPackages = with pkgs; [
      ( # This script should be run before changing the version of postgresql
        # TODO: make a postgresql file, move this and other settings of and dependencies on postgres there.
        let
          # TODO: specify the postgresql package you'd like to upgrade to.
          # Do not forget to list the extensions you need.
          newPostgres = pkgs.postgresql_17.withPackages (pp: [
            # pp.plv8
          ]);
          cfg = config.services.postgresql;
        in
          pkgs.writeScriptBin "upgrade-pg-cluster" ''
        set -eux
        # XXX it's perhaps advisable to stop all services that depend on postgresql
        systemctl stop postgresql

        export NEWDATA="/var/lib/postgresql/${newPostgres.psqlSchema}"
        export NEWBIN="${newPostgres}/bin"

        export OLDDATA="${cfg.dataDir}"
        export OLDBIN="${cfg.finalPackage}/bin"

        install -d -m 0700 -o postgres -g postgres "$NEWDATA"
        cd "$NEWDATA"
        sudo -u postgres "$NEWBIN/initdb" -D "$NEWDATA" ${lib.escapeShellArgs cfg.initdbArgs}

        sudo -u postgres "$NEWBIN/pg_upgrade" \
          --old-datadir "$OLDDATA" --new-datadir "$NEWDATA" \
          --old-bindir "$OLDBIN" --new-bindir "$NEWBIN" \
          "$@"
          ''
      )
    ];
  };
}

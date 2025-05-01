#!/usr/bin/env bash

if [ "$(whoami)" == "archerd" ]; then
    # applets
    xfce4-power-manager &
    # this one doesn't show up unless something is in the print queue.
    system-config-printer-applet &

    # software to be autostarted
    if type yubioath-desktop; then
        yubioath-desktop &
    else
        $HOME/.dotfiles/yubico-icon-generator.sh &
    fi
fi


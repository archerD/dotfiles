#!/usr/bin/env bash

if [ "$(whoami)" == "archerd" ]; then
    # tray for applets and application notifications
    # trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 63 --tint 0x145A32 --heighttype request --margin 3 --padding 3 &
    # stalonetray --config /home/archerd/.dotfiles/stalonetrayrc &
    # trayer --edge top --align right --widthtype request --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x294029 --expand true --padding 3 --monitor primary --iconspacing 2 \
    #     --heighttype pixel --height 38 &

    # applets (sleep to force desired order)
    xfce4-power-manager &
    if type nm-applet; then
        nm-applet --sm-disable & # not found (I think it's autolaunching on nixos)
    fi
    blueman-applet & # erroring out on nixos
    pasystray &
    # kdeconnect-indicator & # using home-manager for this
    # this one doesn't show up unless something is in the print queue.
    system-config-printer-applet &

    # to get the aliases for some stuff.
    #/home/archerd/.dotfiles/aliases
    # software to be autostarted
    if type yubioath-desktop; then
        yubioath-desktop &
    else
        $HOME/.dotfiles/yubico-icon-generator.sh &
    fi
fi


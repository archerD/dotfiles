#! /usr/bin/env bash
xss-lock -l -- env \
    XSECURELOCK_SAVER=saver_xscreensaver \
    XSECURELOCK_AUTH_TIMEOUT=10 \
    XSECURELOCK_KEY_XF86AudioPlay_COMMAND="playerctl -p playerctld play-pause" \
    XSECURELOCK_KEY_XF86AudioPrev_COMMAND="playerctl -p playerctld previous" \
    XSECURELOCK_KEY_XF86AudioNext_COMMAND="playerctl -p playerctld next" \
    XSECURELOCK_KEY_XF86AudioStop_COMMAND="playerctl -p playerctld stop" \
    XSECURELOCK_KEY_XF86AudioMute_COMMAND="amixer set Master toggle" \
    XSECURELOCK_KEY_XF86AudioLowerVolume_COMMAND="amixer set Master 2%-" \
    XSECURELOCK_KEY_XF86AudioRaiseVolume_COMMAND="amixer set Master 2%+" \
    XSECURELOCK_PASSWORD_PROMPT="time" \
    XSECURELOCK_SHOW_DATETIME=1 \
    XSECURELOCK_DATETIME_FORMAT="(%a) %F T %R:%S%z (%Z)" \
    xsecurelock

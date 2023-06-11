#!/bin/sh

SLEEP_FOR=5
if [ $# -ne 0 ]
then
    SLEEP_FOR=$1
fi

sleep "$SLEEP_FOR"m
ACTION=$(dunstify --action="snooze1,Snooze 1 Min" --action="snooze10,Snooze 10 Min" --urgency=critical "Sleep Reminder" "Remember to go to bed you idiot")

case "$ACTION" in
    "snooze1")
        ~/.dotfiles/remind-me-to-sleep.sh 1
        ;;
    "snooze10")
        ~/.dotfiles/remind-me-to-sleep.sh 10
        ;;
    "2")
        ;;
esac


#! /usr/bin/env nix-shell
#! nix-shell -i bash -p yad
yad --notification --image="$HOME/.dotfiles/images/yubico-icon.png" --icon-size=32 --command="yubioath-flutter" --text="Yubico Authenticator"

#!/bin/sh

if ! command -v nix 2>&1 >/dev/null
then
    curl -sSf -L https://install.lix.systems/lix | sh -s -- install
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if [ ! -f ~/.ssh/id_ed25519.pub ]
then
    ssh-keygen -ted25519
fi

echo key copped to clipboard. Make sure it is added to github!

TMP_CB=$(pbpaste)
cat ~/.ssh/id_ed25519.pub | pbcopy
read -p "Press enter to continue"
echo "$TMP_CB\c" | pbcopy

if [ ! -d ~/.config/nix ]; then
mkdir ~/.config

nix run nixpkgs#git clone git@github.com:yobson/nix-mac.git ~/.config/nix
fi

cd ~/.config/nix

read -p "Enter hostname:" host
sudo scutil --set HostName $host
sudo scutil --set LocalHostName $host
echo "$host\c" | pbcopy

vim flake.nix

read -p "Press enter to install"
nix run nix-darwin -- switch --flake ~/.config/nix

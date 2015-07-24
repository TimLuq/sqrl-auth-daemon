#!/bin/bash

# copy this file to ~/.sqrl/bin/sqrl-cps and set it to executable
# to see what happens when you connect to 127.0.0.1:25519

echo "started" > ~/.sqrl/daemon.2.log

body=""
for arg in "$@"; do
  body="$body $arg"
done

echo "args: $body" >> ~/.sqrl/daemon.2.log

printf "SQRLCPS\0\0*https://github.com/TimLuq/sqrl-auth-daemon"
notify-send -u critical -i ~/.sqrl/sqrl.svg "A SQRL login request" "$body"

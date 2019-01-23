#!/usr/bin/env sh

export PATH=$PATH:$HOME/.local/bin

# Terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
if [ "$(hostname)" = "wintermute" ]
then
  polybar desktop &
else
  if ! pgrep -x "nm-applet" >/dev/null
  then
    nm-applet &
  fi
  polybar laptop &
fi

echo "Bars launched..."

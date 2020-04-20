#!/usr/bin/env sh

killall polybar

polybar -c ~/.config/polybar/config main &
polybar -c ~/.config/polybar/config secondary &

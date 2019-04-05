#!/usr/bin/env bash

uptime_01=$(uptime | sed -E 's/.*load averages: ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+)/\1/')
uptime_01_int=$(uptime | sed -E 's/.*load averages: ([0-9]+)\.[0-9]+ ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+)/\1/')

uptime_05=$(uptime | sed -E 's/.*load averages: ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+)/\2/')
uptime_05_int=$(uptime | sed -E 's/.*load averages: ([0-9]+\.[0-9]+) ([0-9]+)\.[0-9]+ ([0-9]+\.[0-9]+)/\2/')

uptime_15=$(uptime | sed -E 's/.*load averages: ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+)/\3/')
uptime_15_int=$(uptime | sed -E 's/.*load averages: ([0-9]+\.[0-9]+) ([0-9]+\.[0-9]+) ([0-9]+)\.[0-9]+/\3/')

if [ $uptime_01_int -le 4 ]; then
  printf "#[fg=green]"
elif [ $uptime_01_int -le 8 ]; then
  printf "#[fg=yellow]"
else
  printf "#[fg=red]"
fi

printf $uptime_01
printf " "

if [ $uptime_05_int -le 4 ]; then
  printf "#[fg=green]"
elif [ $uptime_05_int -le 8 ]; then
  printf "#[fg=yellow]"
else
  printf "#[fg=red]"
fi

printf $uptime_05
printf " "

if [ $uptime_15_int -le 4 ]; then
  printf "#[fg=green]"
elif [ $uptime_15_int -le 8 ]; then
  printf "#[fg=yellow]"
else
  printf "#[fg=red]"
fi

printf $uptime_15

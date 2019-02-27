#!/usr/bin/env bash

uptime_01=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 4)
uptime_01_int=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 4 | cut -d. -f1)

uptime_05=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 5)
uptime_05_int=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 5 | cut -d. -f1)

uptime_15=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 6)
uptime_15_int=$(uptime | cut -d ',' -f 3 | cut -d ' ' -f 6 | cut -d. -f1)

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

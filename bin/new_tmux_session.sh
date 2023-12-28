#!/usr/bin/env bash

numberOfArgumentWithoutFirst=$(($# - 1))
evenNumberOfArguments=$(( numberOfArgumentWithoutFirst % 2 ))

if [ $evenNumberOfArguments -ne 0 ]; then
  echo "Wrong number of arguments"
  exit 1
fi

sessionName="Main"
if [ "$1" ]; then
    sessionName=$1
    shift
fi

tmux new-session -s "$sessionName" -n Main -d

windowIndex=1
while (( "$#" >= 2 )); do
  tmux new-window -n "$1" -t "$sessionName"
  tmux send-keys -t "$sessionName":$windowIndex "$2" C-m
  windowIndex=$(( windowIndex + 1 ))
  shift 2
done

tmux attach -t "$sessionName"

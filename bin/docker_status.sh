#!/usr/bin/env bash

docker --version | cut -d " " -f 3 | cut -d "," -f 1

#!/bin/sh

echo "`docker --version | cut -d " " -f 3 | cut -d "," -f 1`"

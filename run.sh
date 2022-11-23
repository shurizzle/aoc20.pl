#!/usr/bin/env bash

swipl -g "['$(dirname "$(readlink -f "$0")")/src/$1.pl']" -g 'solution' -g 'halt'

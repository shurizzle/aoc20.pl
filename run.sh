#!/usr/bin/env bash

exec swipl -g "['$(dirname "$(readlink -f "$0")")/src/$1.pl']" -g 'solution' -g 'halt'

#!/bin/sh

# On MacOS, turn a XML formatted date like "14 Feb 1970, 23:59" to timestamp
date -juf "%d %b %Y, %H:%M" "$1" +%s

#!/bin/sh

status="$2"
artist="$6"
album="${10}"
title="${14}"

printf '%s\t%s\t%s\t%s\n' "$status" "$artist" "$album" "$title" > /tmp/log

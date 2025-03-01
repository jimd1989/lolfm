#!/bin/sh

minutes=$(echo $1 | cut -d ":" -f 1)
seconds=$(echo $1 | cut -d ":" -f 2)
expr $minutes \* 60 + $seconds

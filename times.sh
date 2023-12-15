#!/bin/sh

jq -r --arg day $2 --arg part $3 '.members | map({name, time: (.completion_day_level | getpath([$day, $part]) | .get_star_ts)} | select(.name != null and .time != null)) | sort_by(.time) | map({name, time:(.time | strflocaltime("%H:%M:%S"))}) | map(.name + ": " + .time) | join("\t\t")' < $1

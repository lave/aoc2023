#!/bin/sh

ID=${1:?Leaderbord ID must be specified as the first parameter}
DAY=${2:?Day must be specified as the second parameter}
PART=$3

FILE=$ID.json

if [ -z "$(find . -name $FILE -mmin -60)" ]; then
    if [ -z "$AOC_COOKIE" ]; then
        [ -s .aoc-cookie ] && . .aoc-cookie || { echo "AoC cookie must be defined either in AOC_COOKIE environment variable or .aoc-cookie file"; exit 1; }
    fi

    echo "Downloading $FILE..."

    curl -sf -b "$AOC_COOKIE"  https://adventofcode.com/2023/leaderboard/private/view/$FILE -o $FILE \
        && [ -s $FILE ] || { echo "Failed to download file $FILE from AoC website - AoC cookie is incorrect or expired?"; exit 1; }
fi

function print_times {
    jq -r --arg day $DAY --arg part $1 '.members | map({name, time: (.completion_day_level | getpath([$day, $part]) | .get_star_ts)} | select(.name != null and .time != null)) | sort_by(.time) | map({name, time:(.time | strflocaltime("%H:%M:%S"))}) | map(.name + ": " + .time) | join("\t\t")' < $FILE
}

if [ -n "$PART" ]; then
    print_times $PART
else
    print_times 1
    print_times 2
fi

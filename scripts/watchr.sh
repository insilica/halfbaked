#!/bin/bash
usage="$(basename "$0") [-h] /path/to/watch -- must click a window to refresh after starting.

where:
    -h  show this help text"

seed=42
while getopts ':h:' option; do
  case "$option" in
    h) echo "$usage"
       exit
       ;;
    :) printf "missing argument for -%s\n" "$OPTARG" >&2
       echo "$usage" >&2
       exit 1
       ;;
   \?) printf "illegal option: -%s\n" "$OPTARG" >&2
       echo "$usage" >&2
       exit 1
       ;;
  esac
done
shift $((OPTIND - 1))

file=$1
SELECTED_WID=$(($(xwininfo | grep "Window id" | cut -d " " -f 4)))
while inotifywait -r -e close_write $file; do
echo "File changed... Sending F5 to window id=$SELECTED_WID"
CURRENT_WID=$(xdotool getwindowfocus)
xdotool windowactivate $SELECTED_WID
xdotool key F5
xdotool windowactivate $CURRENT_WID
done

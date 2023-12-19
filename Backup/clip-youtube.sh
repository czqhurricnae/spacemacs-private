#!/bin/bash
# Arguments: URL, Time stamp -5 seconds, length of clip, video file name

IFS=$'\n' urls=($(yt-dlp "$1" --print urls))
echo -ss $2 -i "${urls[0]}" -ss $2 -i "${urls[1]}" -ss 5 -map 0:v -map 1:a -c:v libx264 -c:a aac -t $3 $4

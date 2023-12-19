for FILE in *.vtt; do
    BASE=$(basename -s .vtt "$FILE");
    ffmpeg -y -i $FILE $BASE.srt; tt convert -i $BASE.srt -o $BASE.ttml
done

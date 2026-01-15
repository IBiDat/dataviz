downsize () {
  if [ "$#" -lt 2 ]; then
    echo "Usage: downsize <resize_percentage> <file1> [file2 ...]"
    return 1
  fi

  PERC="$1"
  shift
  FILES="$@"

  # Resize images
  mogrify -resize "$PERC" $FILES || return 1

  # Optimize PNGs only
  pngquant --force --ext .png $FILES
}

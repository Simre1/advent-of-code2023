#!/bin/bash

template_file="src/Template.hs"

if [ -n "$1" ]; then
    min_day="$1"
else
    min_day="1"
fi


# Function to confirm before overwriting files
read -r -p "Are you sure you want to apply the template from day $min_day to day 25 (y/n)? " response
case "$response" in
  [yY]|[yY][eE][sS]) 
      echo "Applying template..." ;; # Continue with overwriting
  *)
      exit 1 ;; # Cancel overwriting
esac

for ((i = $min_day; i <= 25; i++)); do
  day_file="src/Day$i/Main.hs"
  cat "$template_file" > "$day_file"
  sed -i "s/Template/Day$i.Main/g" $day_file
done

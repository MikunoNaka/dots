#!/bin/sh


# ext="gif" # without the dot
echo "enter extention without the period"
read ext
output=1
for i in $(cat $ext.txt); do
	curl "{$i}" --output /zt/Docs/pics/$output.$ext
	# echo "{$i} /pics/$output.$ext"
	((output=$output+1))
done

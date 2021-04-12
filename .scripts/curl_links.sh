#!/bin/sh

output=1
ext="gif" # without the dot
for i in $(cat $ext.txt); do
	curl "{$i}" --output /zt/Docs/pics/$output.$ext
	# echo "{$i} /pics/$output.$ext"
	((output=$output+1))
done

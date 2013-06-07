#!/bin/sh

# CallPrefix CallPostfix Count Prefix

files=
i=1

while [ $3 -ge $i ]
do
	f=$4_$i.gm
	files="$files $f"
	i=$(expr $i + 1)
done

$1 $files $2

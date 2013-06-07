#!/bin/sh

# RandomFunc Count Prefix

files=
i=1

while [ $2 -ge $i ]
do
	f=$3_$i.gm
	echo Creating $f...
	$1 > $f
	files="$files $f"
	i=$(expr $i + 1)
done

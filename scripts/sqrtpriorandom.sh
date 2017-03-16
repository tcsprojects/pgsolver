#!/bin/sh

function awful_sqrt
{
	i=0
	j=0
	while [ $1 -ge $j ]
	do
		i=$(expr $i + 1)
		j=$(expr $j + $i + $i + 1)
	done
	echo $i
}

$1 $2 $(awful_sqrt $2) $3 $4 $5 $6 $7 $8 $9

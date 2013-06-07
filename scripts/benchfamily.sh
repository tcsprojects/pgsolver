#!/bin/sh

# PrefixCall SuffixCall Benchmark Params LowerBound UpperBound

function call #string
{
	$2 $1 $3 | $4 -s -gp -n $1 $5
}

m=$5

while [ $6 -ge $m ]
do
	call $m "$1" "$2" "$3" "$4"
	m=$(expr $m + 1)
done

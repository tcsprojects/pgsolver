#!/bin/sh

# PrefixCall SuffixCall Benchmark Params LowerBound UpperBound Multiplier Count BenchRandom.Sh

function call #string
{
	$7 "$2 $1 $3" $6 "$4" "$5 -s -gp -n $1"
}

m=$5

while [ $6 -ge $m ]
do
	a=$(expr $m "*" $7)
	call "$a" "$1" "$2" "$3" "$4" $8 $9
	m=$(expr $m + 1)
done

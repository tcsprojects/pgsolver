#!/bin/sh

# RandomFunc Count Benchmark BenchParams

randomfunc=$1
count=$2
benchmark=$3
benchparams=$4

files=
i=0

while [ $count -ge $i ]
do
	f=tempbench_$RANDOM.temp
	$randomfunc > $f
	files="$files $f"
	i=$(expr $i + 1)
done

$benchmark $files $benchparams

rm $files
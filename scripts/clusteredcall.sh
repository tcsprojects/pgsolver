#!/bin/sh

function awful_sqrt
{
	i=0
	j=0
	while [ $1 -ge $j ]
	do
		i=$(expr $i + 1)
		j=$(expr $i \* $i)
	done
	echo $i
}

function awful_log
{
	i=1
	j=10
	while [ $1 -ge $j ]
	do
		i=$(expr $i + 1)
		j=$(expr 10 \* $j)
	done
	echo $i
}

clust=$1
n=$2

# n sqrt(n) 1 4 log(n) log(n) 2*log(n) sqrt(n) 2*sqrt(n)

sqrt=$(awful_sqrt $n)
sqrtx=$(expr 2 \* $sqrt)
log=$(awful_log $n)
sqrtxl=$(expr $log \* $sqrt)
logx=$(expr 2 \* $log)
logxx=$(expr 2 \* $logx)
logxxx=$(expr 2 \* $logxx)
sqrtlog=$(awful_log $sqrt)
loglog=$(awful_log $log)
b=$(expr $log / $logsqrt + 1)

$clust $n $sqrt 1 4 $b $sqrtlog $log $sqrt $sqrtxl

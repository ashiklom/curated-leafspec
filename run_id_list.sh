#!/bin/bash

file=${1:-all_ids.txt}

while read l; do
    qsub -N $1 -v ID=\'$l\' run-inversion.sh
done < $file

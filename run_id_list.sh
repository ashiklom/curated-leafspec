#!/bin/bash
trap "exit" INT

file=${1:-'all_ids.txt'}

while read l; do
    #qsub -N $l -v ID=$l run-inversion.sh
    ID=$l ./run-inversion.sh
done < $file

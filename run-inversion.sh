#!/bin/bash -l

#$ -q "geo*"
#$ -j y
#$ -o logs/
#$ -l h_rt=12:00:00
#$ -pe omp 5
#$ -V

Rscript invert.R $ID

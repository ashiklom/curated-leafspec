#!/bin/bash

while read r; do
    qdel $r
done < runs.txt

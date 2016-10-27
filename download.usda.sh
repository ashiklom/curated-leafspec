#!/bin/bash

nstep=5000
offset=0

while [ $offset -lt 100000 ]; do
    curl "https://plantsdb.xyz/search?limit=5000&offset=$offset" >> usda_full.json
    let offset+=5000
done

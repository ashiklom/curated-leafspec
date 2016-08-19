SOURCES := $(wildcard process.*.R)
TARGETS := $(patsubst process.%.R, processed-spec-data/%.rds, $(SOURCES))

.PHONY: all clean

all: $(TARGETS)

processed-spec-data/%.rds: process.%.R
	Rscript $<

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf processed-spec-data/*
	

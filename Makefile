SOURCES := $(wildcard process.*.R)
TARGETS := $(patsubst process.%.R, processed-spec-data/%.RData, $(SOURCES))

.PHONY: all clean

all: $(TARGETS)

processed-spec-data/%.RData: .last.install process.%.R
	Rscript $(word 2, $^)

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf processed-spec-data/
	

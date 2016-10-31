DATA := accp angers arctic_chl lopex nasa_fft ngee_arctic ngee_tropics yang_pheno
TARGETS := $(DATA:%=processed-spec-data/%.rds)

.PHONY: all clean

all: $(TARGETS)

processed-spec-data/%.rds: process.%.R
	Rscript $<

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf processed-spec-data/*.rds

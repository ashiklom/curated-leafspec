DATA := accp angers arctic_chl lopex nasa_fft yanghf yangmv
TARGETS := $(patsubst %, processed-spec-data/%.rds, $(DATA))

.PHONY: all clean purge

all: $(TARGETS)

schema:
	sqlite3 specdb.sqlite < schema.sql

processed-spec-data/%.rds: process.%.R
	Rscript $<

processed-spec-data/yanghf.rds processed-spec-data/yangmv.rds: process.yang.R
	Rscript $<

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf specdb.sqlite
	

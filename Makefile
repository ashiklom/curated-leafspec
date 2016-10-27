DATA := nasa_fft #lopex angers
#DATA := accp angers arctic_chl lopex nasa_fft yanghf yangmv

.PHONY: all clean purge schema

all: $(DATA)

%: process.%.R schema species traits
	Rscript $<
	touch .$@

global: 01.populate_global.R raw/bety.species.csv traitInfo.csv schema
	Rscript 01.populate_global.R

schema:
	sqlite3 specdb.sqlite < schema.sql

clean:
	rm -rf 00-run-inversion.sh specdb.sqlite



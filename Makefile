DATA := nasa_fft lopex angers
#DATA := accp angers arctic_chl lopex nasa_fft yanghf yangmv

.PHONY: all clean purge schema

all: $(DATA)

%: process.%.R schema species traits
	Rscript $<

species: 01.populate.species.R raw/bety.species.csv
	Rscript 01.populate.species.R

traits: 01.populate.traits.R traitInfo.csv
	Rscript 01.populate.traits.R

schema:
	sqlite3 specdb.sqlite < schema.sql

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf specdb.sqlite


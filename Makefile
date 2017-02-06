DATA:= accp lopex angers divittorio_conifer nasa_fft ngee_arctic ngee_tropics 
#yang_pheno 
#TARGETS := $(DATA:%=processed-spec-data/%.rds)

.PHONY: all clean reset

all: install reset $(DATA)

processed-spec-data/%.rds: process.%.R
	Rscript $<

clean:
	rm -rf 00-run-inversion.sh

purge: clean
	rm -rf processed-spec-data/*.rds

install:
	Rscript -e "library(devtools); document('specprocess'); install('specprocess')" 

reset:
	./00.wipe_schema.sh
	Rscript 01.projects_table.R
	Rscript 02.species_table.R
	Rscript 03.species_dict.R

%: process.%.R
	Rscript $<

drop-remote:
	ssh new-testpecan "psql leaf_spectra -c 'DROP ALL TABLES ON SCHEMA public'"

sync:
	pg_dump -C leaf_spectra | bzip2 | ssh new-testpecan "bunzip2 | psql leaf_spectra"

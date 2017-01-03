DATA := accp lopex angers nasa_fft ngee_arctic ngee_tropics
# yang_pheno
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

DATA := accp angers divittorio_conifer foster_beetle lopex nasa_fft nasa_hyspiri ngee_arctic ngee_tropics wu_brazil yang_pheno

.PHONY: all clean reset

all: install reset $(DATA) report

processed-spec-data/%.rds: process.%.R
	Rscript $<

install:
	Rscript -e "library(devtools); document('specprocess'); install('specprocess')" 

reset:
	./00.wipe_schema.sh
	Rscript 02.species_table.R
	Rscript 04.species_attributes.R

%: process.%.R
	Rscript $<

upload:
	rsync -avz --progress leaf_spectra.db geo:~/dietzelab/prospectinversion/scripts

report:
	Rscript -e 'rmarkdown::render("spectra_report.Rmd")'


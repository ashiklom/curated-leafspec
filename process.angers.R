#' ---
#' title: Process ANGERS data
#' author: Alexey Shiklomanov
#' ---
#' 

library(specprocess)
source('common.R')
projectcode <- 'angers'

#projects <- data.table(
    #projectcode = 'angers',
    #description = 'Angers, France spectra from INRA',
    #doi = '10.1016/j.rse.2008.02.012',
    #pointofcontact = 'Feret, Jean-Baptiste',
    #email = 'feretjb@cesbio.cnes.fr')
#merge_with_sql(projects, 'projects')


site_plot <- tibble(
    sitecode = 'angers.INRA',
    sitedescription = 'INRA Centra in Angers, France',
    latitude = 47.47,
    longitude = -0.56) %>%
    mutate(plotcode = sitecode,
           plotdescription = sitedescription) %>%
    db_merge_into(db = specdb, table = 'sites', values = ., 
                  by = 'sitecode', id_colname = 'siteid') %>%
    db_merge_into(db = specdb, table = 'plots', values = ., 
                  by = 'plotcode', id_colname = 'plotid')

#' Set paths
PATH.ANGERS <- file.path("data", "angers")
PATH.spec <- file.path(PATH.ANGERS, "spec")
PATH.chem <- file.path(PATH.ANGERS, "LDB_angers2003.csv")

#' Load chemistry data.
species.rxp <- "(^[[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* *.*"
file.rxp <- "an03r(.{4})[.]txt"
angers.chem <- fread(PATH.chem, header=TRUE) %>%
    setnames('Latin Name', 'speciesdatacode') %>%
    .[, lapply(.SD, replace.na)] %>%
    .[, c('leaf_chla_per_area', 'leaf_chlb_per_area', 'leaf_chltot_per_area',
          'leaf_cartot_per_area', 'leaf_anthocyanin_per_area') :=
        lapply(.SD, ud.convert, 'ug cm-2', 'kg m-2'),
        .SDcols = c('C_a', 'C_b', 'C_ab', 'C_car', 'C_anth')] %>%
    .[, leaf_water_thickness := ud.convert(EWT, 'g cm-2', 'kg m-2')] %>%
    .[, leaf_mass_per_area := ud.convert(LMA, 'g cm-2', 'kg m-2')] %>%
    .[, projectcode := projectcode] %>%
    .[, year := 2003] %>%
    .[, sitecode := site_plot$sitecode] %>%
    .[, plotcode := site_plot$plotcode] %>%
    .[, SampleName := sprintf("%s_%s",
                               gsub(species.rxp, "\\1-\\2", speciesdatacode),
                               gsub(file.rxp, "\\1", Refl_file))] %>%
    .[, samplecode := paste(projectcode, SampleName, year, sep = '|')]

#' Read in reflectance and transmittance data into separate matrices.
message("Reading ANGERS spectra...")

#' Read in reflectance and transmittance data
setkey(angers.chem, samplecode)
refl_list <- list()
trans_list <- list()
for (ID in angers.chem[, unique(samplecode)]) {
    refl_files <- angers.chem[ID, Refl_file]
    refl_files_full <- file.path(PATH.spec, refl_files)
    refl_list[[ID]] <- fread(refl_files_full) %>%
        setnames(c('wavelength', 'spectravalue')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               spectratype = 'reflectance')
    trans_files <- angers.chem[ID, Trans_file] 
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- fread(trans_files_full) %>%
        setnames(c('wavelength', 'spectravalue')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               spectratype = 'transmittance')
}
specdat <- rbind(rbindlist(refl_list), rbindlist(trans_list))

spec_samples <- specdat %>% distinct(samplecode)

chem_samples <- angers.chem %>%
    distinct(samplecode, projectcode, year, sitecode, plotcode, speciesdatacode) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              select(-speciesdictid, -speciesdictcomment) %>% 
              collect %>% 
              setDT) %>%
    select(-speciesdatacode)

samples <- full_join(spec_samples, chem_samples) %>%
    db_merge_into(db = specdb, table = 'samples', values = ., 
                  by = 'samplecode', id_colname = 'sampleid')

spectra_info <- specdat %>% 
    distinct(samplecode, spectratype) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = ., 
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- specdat %>%
    left_join(spectra_info) %>%
    write_spectradata

traits <- angers.chem %>%
    select(samplecode, starts_with('leaf_')) %>% 
    melt(id.vars = 'samplecode', variable.name = 'trait', 
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[grepl('_area|_thickness', trait), unit := 'kg m-2'] %>%
    .[grepl('ratio', trait), unit := 'unitless'] %>%
    db_merge_into(db = specdb, table = 'trait_info', values = ., 
                  by = 'trait', id_colname = 'traitid')

trait_data <- db_merge_into(db = specdb, table = 'trait_data', values = traits, 
                  by = 'samplecode', id_colname = 'traitdataid')

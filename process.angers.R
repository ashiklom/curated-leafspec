#' ---
#' title: Process ANGERS data
#' author: Alexey Shiklomanov
#' ---
#' 

library(specprocess)
specdb <- src_postgres('leaf_spectra')
projectcode <- 'angers'

#projects <- tibble(
    #projectcode = 'angers',
    #description = 'Angers, France spectra from INRA',
    #doi = '10.1016/j.rse.2008.02.012',
    #pointofcontact = 'Feret, Jean-Baptiste',
    #email = 'feretjb@cesbio.cnes.fr')
#merge_with_sql(projects, 'projects')

sites <- tibble(
    code = 'angers.INRA',
    description = 'INRA Centra in Angers, France')
merge_with_sql(sites, 'sites')

plots <- tibble(
    code = sites$code,
    description = sites$description,
    latitude = 47.47,
    longitude = -0.56)
merge_with_sql(plots, 'plots')

#' Set paths
PATH.ANGERS <- file.path("data", "ANGERS")
PATH.spec <- file.path(PATH.ANGERS, "spec")
PATH.chem <- file.path(PATH.ANGERS, "LDB_angers2003.csv")

#' Load chemistry data.
species.rxp <- "(^[[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* *.*"
file.rxp <- "an03r(.{4})[.]txt"
angers.chem <- fread(PATH.chem, header=TRUE) %>%
    setnames('Latin Name', 'datacode') %>%
    .[, lapply(.SD, replace.na)] %>%
    .[, c('leaf_chla_area', 'leaf_chlb_area', 'leaf_chltot_area',
          'leaf_cartot_area', 'leaf_anthocyanin_area') :=
        lapply(.SD, ud.convert, 'ug cm-2', 'kg m-2'),
        .SDcols = c('C_a', 'C_b', 'C_ab', 'C_car', 'C_anth')] %>%
    .[, leaf_water_thickness := ud.convert(EWT, 'g cm-2', 'kg m-2')] %>%
    .[, leaf_mass_per_area := ud.convert(LMA, 'g cm-2', 'kg m-2')] %>%
    .[, projectcode := projectcode] %>%
    .[, year := 2003] %>%
    .[, sitecode := sites$code] %>%
    .[, plotcode := plots$code] %>%
    .[, SampleName := sprintf("%s_%s",
                               gsub(species.rxp, "\\1-\\2", datacode),
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
        setnames(c('wavelength', 'value')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               type = 'reflectance')
    trans_files <- angers.chem[ID, Trans_file] 
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- fread(trans_files_full) %>%
        setnames(c('wavelength', 'value')) %>%
        mutate(samplecode = ID,
               fname = refl_files,
               type = 'transmittance')
}
specdat <- rbind(rbindlist(refl_list), rbindlist(trans_list))

spec_samples <- specdat %>% distinct(samplecode)

chem_samples <- angers.chem %>%
    distinct(samplecode, projectcode, year, sitecode, plotcode, datacode) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              select(-id, -comment) %>% 
              collect %>% 
              setDT) %>%
    select(-datacode)

samples <- full_join(spec_samples, chem_samples) %>% rename(code = samplecode)
merge_with_sql(samples, 'samples')

spectra_info <- specdat %>% distinct(samplecode, type)
merge_with_sql(spectra_info, 'spectra_info')

spectra_data <- specdat %>%
    left_join(tbl(specdb, 'spectra_info') %>%
              select(samplecode, spectraid = id) %>%
              collect %>% 
              setDT)
merge_with_sql(spectra_data, 'spectra_data')

traits <- angers.chem %>%
    select(samplecode, starts_with('leaf_')) %>% 
    melt(id.vars = 'samplecode', variable.name = 'trait', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[grepl('_area|_thickness', trait), unit := 'kg m-2'] %>%
    .[grepl('ratio', trait), unit := 'unitless']

merge_with_sql(trait_info, 'trait_info')
merge_with_sql(traits, 'trait_data')

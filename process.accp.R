library(specprocess)

project.code <- 'accp'

accp_path <- 'data/accp'

# Load all chemistry data
traits_path <- file.path(accp_path, "leafchem")
traits_files <- list.files(traits_path, "chem.dat$")
traits_list <- list()
for (f in traits_files){
    print(f)
    fname <- file.path(traits_path, f)
    dat <- fread(fname, header=TRUE)
    traits_list[[f]] <- dat
}
traits_dat_raw <- rbindlist(traits_list, fill=TRUE)
colnames_dict <- c('sampleid' = 'samples.code',
                   'site_id' = 'sitecode',
                   'plot_id' = 'plotcode',
                   'species' = 'datacode',
                   'cell' = 'leaf_cellulose_pct_mass',
                   'lignin' = 'leaf_lignin_pct_mass',
                   'carbon' = 'leaf_C_pct_mass',
                   'hydrogen' = 'leaf_H_pct_mass',
                   'nitrogen' = 'leaf_N_pct_mass',
                   'chloro_a' = 'leaf_chla_mass',
                   'chloro_b' = 'leaf_chlb_mass',
                   'npolar' = 'leaf_nonpolar_pct_mass',
                   'polar' = 'leaf_polar_pct_mass',
                   'water_th' = 'leaf_water_thickness',
                   'lutein' = 'leaf_lutein_mass',
                   'neoxanth' = 'leaf_neoxanthin_mass',
                   'beta_car' = 'leaf_betacarotene_mass')
setnames(traits_dat_raw, names(colnames_dict), colnames_dict)

pct_cols <- grep('_pct_', colnames_dict, value = TRUE)
mass_cols <- grep('[^p][^c][^t]_mass$', colnames_dict, value = TRUE)
new_mass_cols <- gsub('_mass$', '_pct_mass', mass_cols)

# Set units for all values
traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)] %>%
    .[, (pct_cols) := lapply(.SD, `*`, ud_units$'%'), .SDcols = pct_cols] %>%
    .[, (mass_cols) := lapply(.SD, `*`, with(ud_units, mg/g)), .SDcols = mass_cols] %>%
    .[, leaf_water_thickness := leaf_water_thickness * parse_unit('g cm-2')] %>%
# Perform unit conversions
    .[, (mass_cols) := lapply(.SD, `*`, 100 * ud_units$'%'), .SDcols = mass_cols] %>%
    setnames(mass_cols, new_mass_cols) %>%
    .[, leaf_water_thickness := 'units<-'(leaf_water_thickness, parse_unit('kg m-2'))] %>%
# Populate additional metadata
    .[, collectiondate := as.Date(as.character(colldate), format = '%y%m%d')] %>%
    .[, year := year(collectiondate)] %>%
    .[, projectcode := project.code] %>%
    .[, fullname := paste(project.code, samples.code, year, sep = '|')] %>%
# `species.code` is already the correct USDA code
    .[, codetype := 'USDA_plants'] %>% # Species codetype
    .[, sitecode := paste0('accp.', sitecode)] %>%
    .[, plotcode := paste0(sitecode, '.', plotcode)]

## Create sites table
specdb <- src_postgres('leaf_spectra')

#dbSendQuery(specdb$con, '
#DELETE FROM plots
#WHERE code LIKE \'accp.%\'
#')

site <- traits_dat_raw %>% 
    distinct(sitecode) %>%
    rename(code = sitecode)
mrg <- merge_with_sql(site, 'sites')

# Create plots table
# TODO: Read in plot data
accp_plots <- traits_dat_raw %>%
    distinct(plotcode, sitecode) %>%
    rename(code = plotcode)
mrg <- merge_with_sql(accp_plots, 'plots')

# Match species
species_dict <- tbl(specdb, 'species_dict') %>%
    filter(projectcode == project.code) %>%
    select(datacode, speciescode) %>%
    collect() %>%
    setDT()
traits_dat_raw <- left_join(traits_dat_raw, species_dict)

# Samples table
samples <- traits_dat_raw %>%
    distinct(fullname, projectcode, year, collectiondate,
             plotcode, speciescode) %>%
    rename(code = fullname) %>%
    mutate(CanopyPosition = 'sun')
mrg <- merge_with_sql(samples, 'samples')

# Load all spectral data
library(reshape2)
spec_path <- file.path(accp_path, "leafspec")
spec_files <- list.files(spec_path, "sp.dat$")
refl_list <- list()
dry_list <- character()
PA_list <- character()
dat_list <- list()
for (f in spec_files) {
    dry <- "fresh"
    type <- 'pseudo-absorbance'
    if (grepl("_d_sp.dat", f)) dry <- "dry"
    if (grepl('jr_f_sp.dat', f)) type <- 'reflectance'
    fname <- file.path(spec_path, f)
    rawdat <- fread(fname)
    dat <- rawdat %>% select(-band)
    dat_melt <- melt(dat, id.vars = 'wavelength', 
                     variable.name = 'samples.code') %>%
        mutate(freshdry = dry,
               fname = f,
               type = type) %>%
        filter(value > -0.5)
    dat_list[[f]] <- dat_melt
}
accp_spec <- rbindlist(dat_list)
rm(dat_list, dat_melt, dat)

specinfo <- accp_spec %>%
    distinct(samples.code, freshdry, type) %>%
    left_join(traits_dat_raw[, .(samples.code, fullname)])
# Doesn't match up...?

# Condense data down to only matching spectra-trait pairs
keep_names <- intersect(names(refl_list), traits_dat_sp[, SampleName])
traits_dat <- traits_dat_sp[SampleName %in% keep_names] %>%
    .[, Reflectance := refl_list[SampleName]] %>%
    .[, FreshDry := dry_list[SampleName]] %>%
    .[, SpecialSpec := PA_list[SampleName]] %>%
    subToCols()

#saveRDS(traits_dat, file = "processed-spec-data/accp.rds")

# Global variables


library(specprocess)
specdb <- src_postgres('leaf_spectra')
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
colnames_dict <- c('sampleid' = 'samplename',
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

traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)] %>%
# Perform unit conversions
    .[, (mass_cols) := lapply(.SD, ud.convert, 'mg g-1', 'percent'), .SDcols = mass_cols] %>%
    setnames(mass_cols, new_mass_cols) %>%
    .[, leaf_water_thickness := ud.convert(leaf_water_thickness, 'g cm-2', 'kg m-2')]

# Load all spectral data
spec_path <- file.path(accp_path, "leafspec")
spec_files <- list.files(spec_path, "sp.dat$")
refl_list <- list()
dat_list <- list()
for (f in spec_files) {
    if (grepl("_d_sp.dat", f)){
        prep <- "dry"
    } else if (grepl('_f_sp.dat', f)) {
        prep <- 'fresh'
    } else if (grepl('_p_sp.dat', f)) {
        prep <- 'powder'
    } else {
        warning('Unknown spectra type')
        next
    }
    if (grepl('jr_f_sp.dat', f)) {
        type <- 'reflectance'
    } else {
        type <- 'pseudo-absorbance'
    }
    fname <- file.path(spec_path, f)
    rawdat <- fread(fname)
    dat <- rawdat %>% select(-band)
    dat_melt <- melt(dat, id.vars = 'wavelength', 
                     variable.name = 'samplename') %>%
        mutate(sampleprep = prep,
               fname = f,
               type = type) %>%
        filter(value > -0.5)    # NA values stored as -1
    dat_list[[f]] <- dat_melt
}
accp_spec <- rbindlist(dat_list)
rm(dat_list, dat_melt, dat)

# Revise bad sample codes
accp_spec <- accp_spec[, samplename := gsub('BH', 'BHI', samplename)] %>%
    filter(!(samplename %in% c('92CWS61BA2A', '92CWS7FRN', '92HFS19RO4'))) %>%
    .[grepl('^(map|df)_', fname), samplename := paste0(samplename, '_', sampleprep)]

# Match species
species_dict <- tbl(specdb, 'species_dict') %>%
    filter(projectcode == project.code) %>%
    select(datacode, speciescode) %>%
    collect() %>%
    setDT()

# Buld whole samples table
all_samples <- accp_spec %>%
    distinct(samplename, sampleprep, type, fname) %>%
    full_join(traits_dat_raw) %>%
    .[, plot_id := as.character(plot_id)] %>%
    .[grepl('^map_', fname), 
      `:=`(site_id = 'SaplingACMA3',
           datacode = 'ACMA3',
           plot_id = NA)] %>%
    .[grepl('^df_', fname), 
      `:=`(site_id = 'SaplingPSME', 
           datacode = 'PSME',
           plot_id = NA)] %>%
# Populate additional metadata
    .[, collectiondate := as.Date(as.character(colldate), format = '%y%m%d')] %>%
    .[, year := year(collectiondate)] %>%
    .[, projectcode := project.code] %>%
    .[, fullname := paste(project.code, samplename, year, sep = '|')] %>%
# `species.code` is already the correct USDA code
    .[, codetype := 'USDA_plants'] %>% # Species codetype
    .[, sitecode := paste0(projectcode, '.', site_id)] %>%
    .[, plotcode := paste0(sitecode, '.', plot_id)] %>%
    left_join(species_dict)
all_samples[, .N, fullname][N > 1]

## Create sites table
site <- all_samples %>% 
    distinct(sitecode) %>%
    rename(code = sitecode)
mrg <- merge_with_sql(site, 'sites')

# Create plots table
# TODO: Read in plot data
accp_plots <- all_samples %>%
    distinct(plotcode, sitecode) %>%
    rename(code = plotcode)
mrg <- merge_with_sql(accp_plots, 'plots')

# Samples table
samples <- all_samples %>%
    distinct(fullname, projectcode, year, collectiondate,
             plotcode, speciescode) %>%
    rename(code = fullname)
mrg <- merge_with_sql(samples, 'samples')

# Instrument
# TODO: Fill in instrument

# Spec method
# TODO: Fill in spec method

# Spec_info table
specinfo <- all_samples %>%
    filter(!is.na(fname)) %>%
    select(samplecode=fullname, type, sampleprep)
mrg <- merge_with_sql(specinfo, 'spectra_info')

specid <- specdb %>% 
    tbl('spectra_info') %>%
    left_join(tbl(specdb, 'samples') %>% 
               select(-comment)) %>%
    filter(projectcode == project.code) %>%
    select(samplecode, spectraid = id) %>%
    collect() %>%
    setDT()

# Spectra table
specdata <- accp_spec %>%
    left_join(all_samples %>% select(samplecode = fullname, samplename)) %>%
    select(samplecode, wavelength, value) %>%
    left_join(specid) %>%
    select(-samplecode)
mrg <- merge_with_sql(specdata, 'spectra_data')

# Traits table
traits <- all_samples %>%
    select(samplecode = fullname, starts_with('leaf_'), -leaf_wgt) %>%
    mutate_if(is.numeric, as.numeric) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait', na.rm = TRUE)

trait_info <- traits %>% 
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[trait == 'leaf_water_thickness', unit := 'kg m-2']

mrg <- merge_with_sql(trait_info, 'trait_info')
mrg <- merge_with_sql(traits, 'trait_data')

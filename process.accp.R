library(specprocess)
source('common.R')
project.code <- 'accp'
accp_path <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/ACCP/accp'

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
                   'species' = 'speciescode',
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

# Perform unit conversions
traits_dat_raw <- traits_dat_raw[, lapply(.SD, replace.na)] %>%
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
        spectratype <- 'reflectance'
    } else {
        spectratype <- 'pseudo-absorbance'
    }
    fname <- file.path(spec_path, f)
    rawdat <- fread(fname)
    dat <- rawdat %>% select(-band)
    dat_melt <- melt(dat, id.vars = 'wavelength', 
                     variable.name = 'samplename') %>%
        mutate(sampleprep = prep,
               fname = f,
               spectratype = spectratype) %>%
        filter(value > -0.5)    # NA values stored as -1
    dat_list[[f]] <- dat_melt
}
accp_spec <- rbindlist(dat_list)
rm(dat_list, dat_melt, dat)

# Revise bad sample codes
accp_spec <- accp_spec[, samplename := gsub('BH', 'BHI', samplename)] %>%
    filter(!(samplename %in% c('92CWS61BA2A', '92CWS7FRN', '92HFS19RO4'))) %>%
    .[grepl('^(map|df)_', fname), samplename := paste0(samplename, '_', sampleprep)]

# Buld whole samples table
all_samples <- accp_spec %>%
    distinct(samplename, sampleprep, spectratype, fname) %>%
    full_join(traits_dat_raw) %>%
    .[, plot_id := as.character(plot_id)] %>%
    .[grepl('^map_', fname), 
      `:=`(site_id = 'SaplingACMA3',
           speciescode = 'ACMA3',
           plot_id = NA)] %>%
    .[grepl('^df_', fname), 
      `:=`(site_id = 'SaplingPSME', 
           speciescode = 'PSME',
           plot_id = NA)] %>%
# Populate additional metadata
    .[, collectiondate := as.Date(as.character(colldate), format = '%y%m%d')] %>%
    .[, year := year(collectiondate)] %>%
    .[, projectcode := project.code] %>%
    .[, fullname := paste(project.code, samplename, year, sep = '|')] %>%
# `species.code` is already the correct USDA code
    .[, sitecode := paste0(projectcode, '.', site_id)] %>%
    .[, plotcode := paste0(sitecode, '.', plot_id)] %>%
    .[speciescode == 'USNEA', speciescode := 'USNEA2'] %>%
    .[speciescode == 'POGR', speciescode := 'POSE']

#species <- tbl(specdb, 'species') %>%
    #select(speciescode) %>%
    #collect() %>%
    #setDT()
#sp <- anti_join(all_samples %>% select(speciescode), species)

## Create sites table
accp_site_info <- tribble(
    ~sitecode, ~sitedescription, ~latitude, ~longitude, ~site_merge_tag,
    'accp.HF', 'Harvard Forest, Petersham, MA', 42.4950, -71.7981, 'leaf',
    'accp.BHI', 'Blackhawk Island, WI', 43.6333, -89.7583, 'leaf',
    'accp.HOW', 'Howland, ME', 45.2222, -68.7356, 'leaf',
    'accp.GAIN', 'Gainesville, FL', 29.7000, -82.1667, 'leaf',
    'accp.JR', 'Jasper Ridge, CA', 37.4111, -121.7631, 'leaf',
    'accp.RICE', 'Dunnigan and Pleasant Grove, CA', mean(c(38.9167, 38.7292)), mean(c(-120.1122, -120.4581)), 'leaf') %>%
    setDT()

site <- all_samples %>% 
    distinct(sitecode) %>%
    left_join(accp_site_info)
site_merge <- db_merge_into(db = specdb, table = 'sites', values = site,
                            by = c('sitecode', 'sitedescription'), id_colname = 'siteid')

# Create plots table
accp_plots <- all_samples %>%
    distinct(plotcode, sitecode) %>%
    left_join(accp_site_info)
plot_merge <- db_merge_into(db = specdb, table = 'plots', values = accp_plots,
                            by = c('plotcode', 'sitecode'), id_colname = 'plotid')

# Samples table
samples <- all_samples %>%
    distinct(fullname, projectcode, year, collectiondate,
             plotcode, speciescode) %>%
    rename(samplecode = fullname)
samples_merge <- db_merge_into(db = specdb, table = 'samples', values = samples,
                     by = c('samplecode'), id_colname = 'sampleid')

# Instrument
specmethods <- tribble(
    ~instrumentname, ~specmethodcomment, ~site_merge_tag,
    'NIRS 6500 laboratory spectrometer', 'ACCP measurement of wet and dry leaves', 'leaf',
    'ACCP field spectrometer', 'ACCP measurement of seedlings in field', 'seedling') %>%
    db_merge_into(db = specdb, table = 'instruments', values = .,
                  by = 'instrumentname', id_colname = 'instrumentid') %>%
    db_merge_into(db = specdb, table = 'specmethods', values = .,
                  by = c('instrumentid', 'specmethodcomment'), 
                  id_colname = 'specmethodid') %>%
    left_join(accp_site_info)

# Spec_info table
specinfo <- all_samples %>%
    filter(!is.na(fname)) %>%
    left_join(setDT(specmethods)) %>%
    select(samplecode=fullname, spectratype, sampleprep, specmethodid) %>%
    db_merge_into(db = specdb, table = 'spectra_info',
                  values = ., by = 'samplecode', 
                  id_colname = 'spectraid')

# Spectra table
specdata <- accp_spec %>%
    left_join(all_samples %>% select(samplecode = fullname, samplename)) %>%
    select(samplecode, wavelength, spectravalue = value) %>%
    left_join(specinfo %>% select(samplecode, spectraid)) %>%
    select(-samplecode) %>%
    write_spectradata

# Traits table
traits <- all_samples %>%
    select(samplecode = fullname, starts_with('leaf_'), -leaf_wgt) %>%
    mutate_if(is.numeric, as.numeric) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait', 
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- traits %>% 
    distinct(trait) %>%
    .[grepl('_pct', trait), unit := '%'] %>%
    .[trait == 'leaf_water_thickness', unit := 'kg m-2'] %>%
    db_merge_into(db = specdb, table = 'trait_info',
                  values = ., by = 'trait', id_colname = 'traitid')

traits <- db_merge_into(db = specdb, table = 'trait_data',
                        values = traits, by = 'samplecode', 
                        id_colname = 'traitdataid')

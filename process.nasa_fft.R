#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup
library(specprocess)
source('common.R')

dbGetQuery(specdb$con, 'DELETE FROM projects WHERE projectcode = "nasa_fft"')

projects <- tibble(
    projectcode = 'nasa_fft',
    projectshortname = 'NASA FFT',
    projectdescription = 'NASA Forest Functional Types (FFT)',
    pointofcontact = 'Serbin, Shawn',
    email = 'serbinsh@bnl.gov') %>% 
    write_project()

#' Set paths for FFT data
PATH.FFT <- '~/Dropbox/NASA_TE_PEcAn-RTM_Project/Data/NASA_FFT_Project'

#' # Process reflectance and transmittance data
#' Set reflectance data path
PATH.spec <- file.path(PATH.FFT, "spec")
PATH.refl <- file.path(PATH.spec, "NASA_FFT_LC_Refl_Spectra_v4.csv")

#' Set names for fixing
names_samples <- c("Sample_Name" = "SampleName",
                   "Sample_Year" = "year",
                   "Height" = "CanopyPosition",
                   'Site' = 'sitecode',
                   'Plot' = 'plotcode',
                   'Species' = 'speciesdatacode')

names_specInfo <- c("Instrumentation" = "Instrument",
                    "Measurement_Type" = "Apparatus",
                    "Measurement" = "SpectraType")

names_all <- c(names_samples, names_specInfo)

#' Load reflectance data 
nasa_fft.all_refl <- fread(PATH.refl, header=TRUE) %>%
    mutate(projectcode = projects[['projectcode']],
           samplecode = paste(projectcode, Sample_Name, Sample_Year,
                              sep = '|')) %>%
    setnames(names(names_all), names_all)

if (any(duplicated(nasa_fft.all_refl[, samplecode]))) {
    stop("samplecode is not unique")
}

#' Isolate full sample information

#' Sort out needle age variables
fixNeedleAge <- function(dat) {
    age.pine <- c("N", "O")
    age.conifer <- c("N", 2, 3)
    out <- dat %>%
        mutate(NeedleOldNew = ifelse(Age %in% age.pine, Age, NA),
            NeedleAge = ifelse(Age %in% age.conifer, Age, NA)) %>%
        mutate(NeedleAge = ifelse(NeedleAge == "N", 1, NeedleAge)) %>%
        mutate(NeedleAge = as.numeric(NeedleAge))
    return(out)
}

samples_refl <- select(nasa_fft.all_refl, -starts_with('Wave_')) %>%
    fixNeedleAge()
spectra_refl <- select(nasa_fft.all_refl, samplecode, starts_with('Wave_')) %>%
    melt(id.vars = 'samplecode', value.name = 'spectravalue') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', variable))) %>%
    select(-variable)
spectra_info_refl <- distinct(spectra_refl, samplecode) %>%
    mutate(spectratype = 'reflectance')

#' Repeat above steps for transmittance
message("Loading transmittance...")
PATH.trans <- file.path(PATH.spec, "NASA_FFT_IS_Tran_Spectra_v4.csv")
nasa_fft.trans_all <- fread(PATH.trans, header=TRUE) %>%
    setnames(names(names_all), names_all) %>%
    mutate(samplecode = paste(projects[['projectcode']], SampleName, year, 
                              sep = '|'))

samples_trans <- select(nasa_fft.trans_all,
                        samplecode, SampleName, year,
                        sitecode, plotcode, speciesdatacode,
                        CanopyPosition, Age) %>%
    fixNeedleAge()
spectra_trans <- select(nasa_fft.trans_all, samplecode, starts_with('Wave_')) %>%
    melt(id.vars = 'samplecode', value.name = 'spectravalue') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', variable))) %>%
    select(-variable)
spectra_info_trans <- distinct(spectra_trans, samplecode) %>%
    mutate(spectratype = 'transmittance')

#' # Process chemistry data
#' Set paths
PATH.d15N <- file.path(PATH.FFT, 
                       "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv")
PATH.lignin <- file.path(PATH.FFT,
                         "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv")
PATH.CN <- file.path(PATH.FFT,
                     "NASA_FFT_Project_CN_Data_4R.csv")
PATH.SLA_LMA <- file.path(PATH.FFT,
                          "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv")

#' Read in data
nasa_fft.d15N <- fread(PATH.d15N, header=TRUE) %>%
    setnames('SPECIES', 'speciesdatacode')
nasa_fft.lignin <- fread(PATH.lignin, header=TRUE) %>%
    setnames('SPECIES', 'speciesdatacode')
nasa_fft.cn <- fread(PATH.CN, header=TRUE) %>%
    setnames('Species', 'speciesdatacode')
nasa_fft.lma <- fread(PATH.SLA_LMA, header=TRUE) %>%
    setnames('Species', 'speciesdatacode')

#' Remove data with comments, which usually indicate that there's something 
#' wrong with the data. TODO: Look more closely into this.
nasa_fft.d15N <- nasa_fft.d15N[COMMENTS == ""]
nasa_fft.lignin <- nasa_fft.lignin[COMMENTS == ""]

#' Set negative values of LMA to NA. Negative values make no sense for EWT and 
#' LMA.
nasa_fft.lma <- nasa_fft.lma %>%
    mutate(EWT_g_cm2 = if_else(EWT_g_cm2 < 0, NA_real_, EWT_g_cm2),
           LMA_g_DW_m2 = if_else(LMA_g_DW_m2 < 0, NA_real_, LMA_g_DW_m2))

#' Extract only the values that we're interested in from each data.table.
mergeby.lower <- c("Sample_Name", "Sample_Year", 'speciesdatacode')
mergeby.caps <- c('SAMPLE_NAME', 'SAMPLE_YEAR', 'speciesdatacode')
nasa_fft.d15N <- nasa_fft.d15N[, c(mergeby.caps, "SAMPLE_dN15"), with=F]
nasa_fft.lignin <- nasa_fft.lignin[, c(mergeby.caps, "ADF_PERC_DW", "ADL_PERC_DW",
                             "CELL_PERC_DW"), with=F]
nasa_fft.cn <- nasa_fft.cn[, c(mergeby.lower, "Perc_N", "Perc_C", "CNRatio"), with=F]
nasa_fft.lma <- nasa_fft.lma[, c(mergeby.lower, "EWT_g_cm2", "LMA_g_DW_m2"), with=F]

#' Remove duplicates from each data set by averaging over them. Keeping the 
#' duplicates in there makes it difficult to eventually merge the data tables.
remove.duplicates <- function(x){
    if(is.numeric(x)) return(mean(x, na.rm=TRUE))
    return(x[1])
}
nasa_fft.d15N <- nasa_fft.d15N[, lapply(.SD, remove.duplicates), by=mergeby.caps]
check.unique(nasa_fft.d15N, mergeby.caps)
nasa_fft.lignin <- nasa_fft.lignin[, lapply(.SD, remove.duplicates), by=mergeby.caps]
check.unique(nasa_fft.lignin, mergeby.caps)
nasa_fft.cn <- nasa_fft.cn[, lapply(.SD, remove.duplicates), by=mergeby.lower]
check.unique(nasa_fft.cn, mergeby.lower)
nasa_fft.lma <- nasa_fft.lma[, lapply(.SD, remove.duplicates), by=mergeby.lower]
check.unique(nasa_fft.lma, mergeby.lower)

#' Merge nasa_fft data together.
setkeyv(nasa_fft.d15N, mergeby.caps)
setkeyv(nasa_fft.lignin, mergeby.caps)
setkeyv(nasa_fft.cn, mergeby.lower)
setkeyv(nasa_fft.lma, mergeby.lower)
merge.caps <- merge(nasa_fft.d15N, nasa_fft.lignin, all=T)
merge.lower <- merge(nasa_fft.cn, nasa_fft.lma, all=T)
setnames(merge.caps, mergeby.caps, mergeby.lower)
nasa_fft.traits <- merge(merge.caps, merge.lower, by=mergeby.lower, all=T) %>%
    mutate(leaf_water_thickness = ud.convert(EWT_g_cm2, 'g cm-2', 'kg m-2'),
           leaf_mass_per_area = ud.convert(LMA_g_DW_m2, 'g m-2', 'kg m-2')) %>%
    rename(leaf_C_pct_mass = Perc_C,
           leaf_N_pct_mass = Perc_N,
           leaf_CN_ratio_mass = CNRatio,
           leaf_cellulose_pct_mass = CELL_PERC_DW,
           leaf_lignin_pct_mass = ADL_PERC_DW,
           leaf_fiber_pct_mass = ADF_PERC_DW,
           leaf_dN15 = SAMPLE_dN15,       # TODO: Figure out unit!
           SampleName = Sample_Name,
           year = Sample_Year) %>%
    mutate(samplecode = paste(projects[['projectcode']], SampleName, year, sep = '|'))
samples_traits <- nasa_fft.traits %>%
    select(samplecode, SampleName, year, speciesdatacode)

#' # Set up SQL tables
#n_dup <- function(dat) print(sum(duplicated(dat$samplecode)))
samples_all <- samples_refl %>%
    left_join(anti_join(samples_trans, samples_refl, 'samplecode')) %>%
    full_join(samples_traits) %>%
# Missing species:
    # GRAS -- Shawn thinks it's just "grass" of an unknown species
    # SORI -- Unknown site (MW), unknown species
    #filter(!speciesdatacode %in% c('SORI', 'GRAS')) %>%   # Unknown species
    left_join(read_csv('data/species_dict/nasa_fft_species_dict.csv') %>% setDT()) %>% 
    .[is.na(plotcode), plotcode := stringr::str_extract(SampleName, "^[[:alnum:]]+")] %>%
    .[is.na(sitecode), sitecode := stringr::str_extract(plotcode, "^[[:alpha:]]+")] %>%
# This is probably Madison, WI
    #filter(sitecode != 'MW') %>% 
    mutate(projectcode = projects[['projectcode']],
           sitecode = paste(projectcode, sitecode, sep = '.'),
           plotcode = paste(projectcode, plotcode, sep = '.'))

#samples_all %>%
    #group_by(samplecode) %>%
    #summarize(n = n()) %>%
    #filter(n > 1)

## Code to examine missing species
#samples_refl %>% distinct(speciesdatacode)
#samples_trans %>% distinct(speciesdatacode)
#samples_all %>% 
    #filter(is.na(speciescode)) %>% 
    #select(speciesdatacode, sitecode, samplecode)
#sp <- samples_all %>% 
    #filter(is.na(speciescode)) %>%
    #select(speciesdatacode)

ndup <- function(dat, colname) print(sum(duplicated(dat[[colname]])))
sites <- distinct(samples_all, sitecode) %>% write_sites()

plots <- read_csv(file.path(PATH.FFT, 'Stand_Info', 'Plot_Locations', 
                            'Aggregated_N_Coords_ALL.csv')) %>%
    distinct(PLOT, SITE, LAT, LON) %>%
    rename(latitude = LAT, longitude = LON) %>%
    mutate(projectcode = projects[['projectcode']],
           sitecode = paste(projectcode, SITE, sep = '.'),
           plotcode = paste(projectcode, PLOT, sep = '.')) %>%
    setDT() %>%
    right_join(samples_all %>% distinct(sitecode, plotcode)) %>%
    write_plots()

## Code to examine missing plots
#semi <- plots %>% semi_join(samples_all)
#sp <- anti_join(samples_all, plots) %>% distinct(sitecode, plotcode)
#spdat <- sp %>% 
    #left_join(samples_all) %>%
    #group_by(sitecode) %>%
    #count()
#ps <- anti_join(plots, samples_all) %>% distinct(plotcode)
    
samples <- samples_all %>%
    select(projectcode, samplecode, year, 
           sitecode, plotcode, speciescode) %>%
    db_merge_into(db = specdb, table = 'samples', values = ., by = c('samplecode'))

condition_info <- tribble(
    ~condition, ~conditiondescription,
    'CanopyPosition', 'Position in canopy (bottom, middle, top)',
    'NeedleOldNew', 'Whether needles are newly grown (new; age < 1) or not (old; age > 1)',
    'NeedleAge', 'Needle age in years') %>%
    db_merge_into(db = specdb, table = 'sample_condition_info', values = .,
                  by = c('condition', 'conditiondescription'))

sample_condition <- samples_all %>%
    select(samplecode, CanopyPosition, NeedleOldNew, NeedleAge) %>%
    melt(id.vars = 'samplecode', variable.name = 'condition',
         value.name = 'conditionvalue', na.rm = TRUE) %>%
    db_merge_into(db = specdb, table = 'sample_condition', values = ., by = 'samplecode')


instruments <- tibble(instrumentcode = 'asd-fieldspec-3',
                      instrumentname = 'ASD FieldSpec 3') %>% 
    db_merge_into(db = specdb, table = 'instruments', values = ., by = 'instrumentcode')

specmethods <- tribble(
    ~specmethodcode, ~spectratype, ~apparatus, ~calibration,
    'nasa_fft-refl', 'reflectance', 'Leaf clip', 'Spectralon ratio',
    'nasa_fft-trans', 'transmittance', 'Integrating sphere', NA) %>%
    mutate(instrumentcode = instruments$instrumentcode) %>%
    db_merge_into(db = specdb, table = 'specmethods', values = ., by = 'specmethodcode') %>%
    setDT()

spectra_info <- full_join(spectra_info_refl, spectra_info_trans) %>%
    left_join(specmethods %>% select(spectratype, specmethodcode)) %>%
    db_merge_into(db = specdb, table = 'spectra_info', values = .,
                  by = c('samplecode', 'spectratype'), id_colname = 'spectraid')

spectra_data <- spectra_refl %>%
    left_join(spectra_trans) %>%
    left_join(spectra_info) %>%
    select(-samplecode) %>%
    write_spectradata()

traits <- nasa_fft.traits %>%
    select(samplecode, starts_with('leaf')) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait',
         value.name = 'traitvalue', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    mutate(unit = case_when(grepl('pct_mass', .$trait) ~ '%',
                            grepl('_per_area|thickness', .$trait) ~ 'kg m-2',
                            grepl('_ratio', .$trait) ~ 'unitless',
                            TRUE ~ NA_character_)) %>%
    db_merge_into(db = specdb, table = 'trait_info', values = ., by = 'trait')

traits <- db_merge_into(db = specdb, table = 'trait_data', values = traits,
                        by = c('samplecode', 'trait'))

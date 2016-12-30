#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup
library(specprocess)
projectcode <- "nasa_fft"
specdb <- src_postgres('leaf_spectra')

#' Set paths for FFT data
PATH.FFT <- file.path("raw","NASA_FFT")

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
                   'Species' = 'datacode')

names_specInfo <- c("Instrumentation" = "Instrument",
                    "Measurement_Type" = "Apparatus",
                    "Measurement" = "SpectraType")

names_all <- c(names_samples, names_specInfo)

#' Load reflectance data 
nasa_fft.all_refl <- fread(PATH.refl, header=TRUE) %>%
    mutate(projectcode = projectcode,
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
    melt(id.vars = 'samplecode') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', variable))) %>%
    select(-variable)
spectra_info_refl <- distinct(spectra_refl, samplecode) %>%
    mutate(type = 'reflectance')

#' Repeat above steps for transmittance
message("Loading transmittance...")
PATH.trans <- file.path(PATH.spec, "NASA_FFT_IS_Tran_Spectra_v4.csv")
nasa_fft.trans_all <- fread(PATH.trans, header=TRUE) %>%
    setnames(names(names_all), names_all) %>%
    mutate(samplecode = paste(projectcode, SampleName, year, 
                              sep = '|'))

samples_trans <- select(nasa_fft.trans_all,
                        samplecode, SampleName, year,
                        sitecode, plotcode, datacode,
                        CanopyPosition, Age) %>%
    fixNeedleAge()
spectra_trans <- select(nasa_fft.trans_all, samplecode, starts_with('Wave_')) %>%
    melt(id.vars = 'samplecode') %>%
    mutate(wavelength = as.numeric(gsub('Wave_', '', variable))) %>%
    select(-variable)
spectra_info_trans <- distinct(spectra_trans, samplecode) %>%
    mutate(type = 'transmittance')

#' # Process chemistry data
#' Set paths
PATH.FFT <- file.path("raw/NASA_FFT")
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
    setnames('SPECIES', 'datacode')
nasa_fft.lignin <- fread(PATH.lignin, header=TRUE) %>%
    setnames('SPECIES', 'datacode')
nasa_fft.cn <- fread(PATH.CN, header=TRUE) %>%
    setnames('Species', 'datacode')
nasa_fft.lma <- fread(PATH.SLA_LMA, header=TRUE) %>%
    setnames('Species', 'datacode')

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
mergeby.lower <- c("Sample_Name", "Sample_Year", 'datacode')
mergeby.caps <- c('SAMPLE_NAME', 'SAMPLE_YEAR', 'datacode')
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
    mutate(samplecode = paste(projectcode, SampleName, year, sep = '|'))
samples_traits <- nasa_fft.traits %>%
    select(samplecode, SampleName, year)

#' # Set up SQL tables
samples_all <- samples_refl %>%
    left_join(anti_join(samples_trans, samples_refl, 'samplecode')) %>%
    full_join(samples_traits) %>%
    left_join(tbl(specdb, 'species_dict') %>% 
              select(-id) %>% 
              collect() %>% 
              setDT()) %>%
    select(-datacode) %>%
    mutate(projectcode = 'nasa_fft',
           sitecode = paste(projectcode, sitecode, sep = '.'),
           plotcode = paste(sitecode, plotcode, sep = '.'))

sites <- distinct(samples_all, sitecode) %>% 
    rename(code = sitecode)
merge_with_sql(sites, 'sites')

# TODO: Add latitude and longitude
plots <- distinct(samples_all, plotcode) %>%
    rename(code = plotcode)
merge_with_sql(plots, 'plots')

samples <- samples_all %>%
    select(projectcode, code = samplecode, year, 
           sitecode, plotcode, speciescode)
merge_with_sql(samples, 'samples')

spectra_info <- left_join(spectra_info_refl, spectra_info_trans)
merge_with_sql(spectra_info, 'spectra_info', 'samplecode')

specid <- tbl(specdb, 'spectra_info') %>%
    rename(spectraid = id) %>%
    right_join(tbl(specdb, 'samples') %>% 
               filter(projectcode == 'nasa_fft') %>%
               select(samplecode = code)) %>%
    select(spectraid, samplecode) %>%
    collect() %>%
    setDT()

spectra_data <- spectra_refl %>%
    left_join(spectra_trans) %>%
    left_join(specid) %>%
    select(-samplecode)
merge_with_sql(spectra_data, 'spectra_data', 'spectraid')

traits <- nasa_fft.traits %>%
    select(samplecode, starts_with('leaf')) %>%
    melt(id.vars = 'samplecode', variable.name = 'trait', na.rm = TRUE)

trait_info <- traits %>%
    distinct(trait) %>%
    mutate(unit = case_when(grepl('pct_mass', .$trait) ~ '%',
                            grepl('_per_area|thickness', .$trait) ~ 'kg m-2',
                            grepl('_ratio', .$trait) ~ 'unitless',
                            TRUE ~ NA_character_))

merge_with_sql(trait_info, 'trait_info', 'trait')
merge_with_sql(traits, 'trait_data', 'samplecode')

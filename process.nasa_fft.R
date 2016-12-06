#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup
source("common.R")

project_code <- "NASA_FFT"

#' Set paths for FFT data
PATH.FFT <- file.path("raw","NASA_FFT")

#' # Process reflectance and transmittance data
#' Set reflectance data path
PATH.spec <- file.path(PATH.FFT, "spec")
PATH.refl <- file.path(PATH.spec, "NASA_FFT_LC_Refl_Spectra_v4.csv")

#' Load reflectance data 
nasa_fft.all_refl <- fread(PATH.refl, header=TRUE) %>%
    .[, Project := project_code] %>%
    .[, FullName := paste(project_code, Sample_Name, Sample_Year,
                             sep = id_separator)] %>%
    setnames("Species", "RawSpecies")

if (any(duplicated(nasa_fft.all_refl[, FullName]))) {
    stop("FullName is not unique")
}

#' Isolate full sample information

all.cols <- colnames(nasa_fft.all_refl)
wl.cols <- grep("Wave_", all.cols, value = TRUE)
info.cols <- all.cols[!(all.cols %in% wl.cols)]
nasa_fft.all <- nasa_fft.all_refl[, info.cols, with = F]

#' Convert spectra into list of matrices

reflFromDT <- function(dat, colname) {
    rowname_vec <- dat[, FullName]
    wl.cols <- grep("Wave_", colnames(dat), value = TRUE)
    wavelengths <- as.numeric(sub("Wave_", "", wl.cols))
    dat[, wl.cols, with = F] %>%
        as.matrix %>% 
        t %>%
        "colnames<-"(rowname_vec) %>%
        apply(2, function(x) list(as.matrix(x))) %>%
        lapply(function(x) cbind(wavelengths, x[[1]])) %>%
        lapply("colnames<-", c("Wavelength", colname)) %>%
        lapply("rownames<-", NULL) %>%
        lapply(specobs)
}

nasa_fft.refl <- reflFromDT(nasa_fft.all_refl, "Reflectance")
nasa_fft.all <- nasa_fft.all[, Reflectance := nasa_fft.refl[FullName]]


#' Correct column names
names_samples <- c("Sample_Name" = "SampleName",
                   "Sample_Year" = "SampleYear",
                   "Height" = "CanopyPosition")

names_specInfo <- c("Instrumentation" = "Instrument",
                    "Measurement_Type" = "Apparatus",
                    "Measurement" = "SpectraType")

names_all <- c(names_samples, names_specInfo)

setnames(nasa_fft.all, names(names_all), names_all)

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

nasa_fft.all <- fixNeedleAge(nasa_fft.all)

#' Repeat above steps for transmittance
message("Loading transmittance...")
PATH.trans <- file.path(PATH.spec, "NASA_FFT_IS_Tran_Spectra_v4.csv")
nasa_fft.trans <- fread(PATH.trans, header=TRUE) %>%
    .[, Project := project_code] %>%
    .[, FullName := paste(Project, Sample_Name, Sample_Year, 
                              sep = id_separator),] %>%
    .[, Transmittance := reflFromDT(., "Transmittance")] %>%
    setnames("Species", "RawSpecies") %>%
    .[, list(FullName, Transmittance, RawSpecies)]

nasa_fft.all <- merge(nasa_fft.all, nasa_fft.trans, 
                      by = c("FullName", "RawSpecies"), 
                      all = TRUE)

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
    setnames('SPECIES', 'RawSpecies')
nasa_fft.lignin <- fread(PATH.lignin, header=TRUE) %>%
    setnames('SPECIES', 'RawSpecies')
nasa_fft.cn <- fread(PATH.CN, header=TRUE) %>%
    setnames('Species', 'RawSpecies')
nasa_fft.lma <- fread(PATH.SLA_LMA, header=TRUE) %>%
    setnames('Species', 'RawSpecies')

#' Remove data with comments, which usually indicate that there's something 
#' wrong with the data. TODO: Look more closely into this.
nasa_fft.d15N <- nasa_fft.d15N[COMMENTS == ""]
nasa_fft.lignin <- nasa_fft.lignin[COMMENTS == ""]

#' Set negative values of LMA to NA. Negative values make no sense for EWT and 
#' LMA.
nasa_fft.lma[EWT_g_cm2 < 0, EWT_g_cm2 := NA]
nasa_fft.lma[LMA_g_DW_m2 < 0, LMA_g_DW_m2 := NA]

#' Extract only the values that we're interested in from each data.table.
mergeby.lower <- c("Sample_Name", "Sample_Year", 'RawSpecies')
mergeby.caps <- c('SAMPLE_NAME', 'SAMPLE_YEAR', 'RawSpecies')
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
nasa_fft.traits <- merge(merge.caps, merge.lower, by=mergeby.lower, all=T)
check.unique(nasa_fft.traits, mergeby.lower)

#' Perform unit conversions
nasa_fft.traits[,leaf_water_content := EWT_g_cm2 * 100^2]  # Convert to g m-2
nasa_fft.traits[,EWT_g_cm2 := NULL]    # Remove unused column
setnames(nasa_fft.traits, "LMA_g_DW_m2", "leaf_mass_per_area")    # No conversion
setnames(nasa_fft.traits, "Perc_C", "leaf_C_percent")      # No conversion
setnames(nasa_fft.traits, "Perc_N", "leaf_N_percent")      # No conversion
setnames(nasa_fft.traits, "CNRatio", "leaf_CN_ratio")  # No conversion
# No protein
setnames(nasa_fft.traits, "CELL_PERC_DW", "leaf_cellulose_percent")
setnames(nasa_fft.traits, "ADL_PERC_DW", "leaf_lignin_percent")
# No starch
setnames(nasa_fft.traits, "ADF_PERC_DW", "leaf_fiber_percent")
setnames(nasa_fft.traits, "SAMPLE_dN15", "leaf_deltaN15")     # TODO: Not sure about this unit
setnames(nasa_fft.traits, c("Sample_Name", "Sample_Year"),
         c("SampleName", "SampleYear"))

#' Combine, organize, and save
traitcols <- c("leaf_water_content", "leaf_mass_per_area", 
               "leaf_C_percent", "leaf_N_percent", "leaf_CN_ratio")
nasa_fft <- merge(nasa_fft.all, nasa_fft.traits,
                  all = TRUE,
                  by = c("SampleName", "SampleYear", 'RawSpecies')) %>% 
    .[is.na(FullName), FullName := paste(project_code, SampleName, SampleYear, 
                                         sep = id_separator)] %>%
    .[, (traitcols) := lapply(.SD, mean, na.rm = TRUE), by = FullName,
      .SDcols = traitcols] %>%
    .[!duplicated(FullName)] %>%
    subToCols

saveRDS(nasa_fft, file = "processed-spec-data/nasa_fft.rds")

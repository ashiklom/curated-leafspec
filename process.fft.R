#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup

#' Source common script.
source("common.R")

#' Set paths for FFT data
PATH.FFT <- file.path("raw","NASA_FFT")
PATH.chem <- file.path(PATH.FFT, "chem")
PATH.spec <-file.path(PATH.FFT, "spec")

#' # Process reflectance and transmittance data
#' Set reflectance data path
PATH.refl <- file.path(PATH.spec, "NASA_FFT_LC_Refl_Spectra_v4.csv")

#' Load reflectance data 
fft.refl <- fread(PATH.refl, header=TRUE)

#' Create sample ID as database + sample_name + year
fft.refl[, sample_id := sprintf("FFT_%s_%s", Sample_Name, Sample_Year)]
check.unique(fft.refl, "sample_id")

#' Isolate full sample information
all.cols <- colnames(fft.refl)
wl.cols <- sprintf("Wave_%d", 350:2500)
info.cols <- all.cols[!(all.cols %in% wl.cols)]
fft.info <- fft.refl[,info.cols, with=F]

#' Load species information file
species.info <- fread(PATH.speciesinfo, header=TRUE)

#' Merge species information with sample information. NOTE that this excludes
#' a few datapoints! TODO: Figure out what they are and how to include them.
setkey(species.info, Label)
setkey(fft.info, Species)
fft.infosp <- fft.info[species.info]

#' Pull out reflectance spectra into its own data.table
fft.reflspec <- fft.refl[, c("sample_id", wl.cols), with=F]

#' Repeat above steps for transmittance, but read in only sample name, year, 
#' and spectra.
PATH.trans <- file.path(PATH.spec, "NASA_FFT_IS_Tran_Spectra_v4.csv")
fft.trans <- fread(PATH.trans, header=TRUE, select=c("Sample_Name", "Sample_Year", wl.cols))
fft.trans[, sample_id := sprintf("FFT_%s_%s", Sample_Name, Sample_Year)]
fft.transspec <- fft.trans[, c("sample_id", wl.cols), with=F]
check.unique(fft.transspec)

#' Tidy up workspace by removing large, unnecessary objects
rm(list = c("all.cols", "fft.refl", "fft.trans", "fft.info"))

#' Summary of important objects so far:
#' * `fft.reflspec` -- data.table containing reflectance spectra
#' * `fft.transspec` -- data.table containing transmittance spectra
#' * `fft.infosp` -- data.table containing information about each sample.


#' # Process chemistry data
#' Set paths
PATH.d15N <- file.path(PATH.FFT, "NASA_FFT_d15N_ANALYZED_DATA_UPDATED_4R.csv")
PATH.lignin <- file.path(PATH.FFT, "NASA_FFT_FIB_LIG_CELL_RESULTS_FINAL_4R.csv")
PATH.CN <- file.path(PATH.FFT, "NASA_FFT_Project_CN_Data_4R.csv")
PATH.SLA_LMA <- file.path(PATH.FFT, "NASA_FFT_SLA_LMA_Data_v2_4R_updated_new.csv")

#' Read in data
fft.d15N <- fread(PATH.d15N, header=TRUE)
fft.lignin <- fread(PATH.lignin, header=TRUE)
fft.cn <- fread(PATH.CN, header=TRUE)
fft.lma <- fread(PATH.SLA_LMA, header=TRUE)

#' Remove data with comments, which usually indicate that there's something 
#' wrong with the data. TODO: Look more closely into this.
fft.d15N <- fft.d15N[COMMENTS == ""]
fft.lignin <- fft.lignin[COMMENTS == ""]

#' Set negative values of LMA to NA. Negative values make no sense for EWT and 
#' LMA.
fft.lma[EWT_g_cm2 < 0, EWT_g_cm2 := NA]
fft.lma[LMA_g_DW_m2 < 0, LMA_g_DW_m2 := NA]

#' Extract only the values that we're interested in from each data.table.
mergeby.lower <- c("Sample_Name", "Sample_Year")
mergeby.caps <- toupper(mergeby.lower)
fft.d15N <- fft.d15N[, c(mergeby.caps, "SAMPLE_dN15"), with=F]
fft.lignin <- fft.lignin[, c(mergeby.caps, "ADF_PERC_DW", "ADL_PERC_DW",
                             "CELL_PERC_DW"), with=F]
fft.cn <- fft.cn[, c(mergeby.lower, "Perc_N", "Perc_C", "CNRatio"), with=F]
fft.lma <- fft.lma[, c(mergeby.lower, "EWT_g_cm2", "LMA_g_DW_m2"), with=F]

#' Remove duplicates from each data set by averaging over them. Keeping the 
#' duplicates in there makes it difficult to eventually merge the data tables.
remove.duplicates <- function(x){
    if(is.numeric(x)) return(mean(x, na.rm=TRUE))
    return(x[1])
}
fft.d15N <- fft.d15N[, lapply(.SD, remove.duplicates), by=mergeby.caps]
check.unique(fft.d15N, mergeby.caps)
fft.lignin <- fft.lignin[, lapply(.SD, remove.duplicates), by=mergeby.caps]
check.unique(fft.lignin, mergeby.caps)
fft.cn <- fft.cn[, lapply(.SD, remove.duplicates), by=mergeby.lower]
check.unique(fft.cn, mergeby.lower)
fft.lma <- fft.lma[, lapply(.SD, remove.duplicates), by=mergeby.lower]
check.unique(fft.lma, mergeby.lower)

#' Merge fft data together.
setkeyv(fft.d15N, mergeby.caps)
setkeyv(fft.lignin, mergeby.caps)
setkeyv(fft.cn, mergeby.lower)
setkeyv(fft.lma, mergeby.lower)
merge.caps <- merge(fft.d15N, fft.lignin, all=T)
merge.lower <- merge(fft.cn, fft.lma, all=T)
setnames(merge.caps, mergeby.caps, mergeby.lower)
fft.chem <- merge(merge.caps, merge.lower, by=mergeby.lower, all=T)
check.unique(fft.chem, mergeby.lower)

#` Sort out missing values
#sample.name.regex <- "^([A-Za-z]+)([0-9]+[B]*)_([A-Za-z]+)_([BMTS]+)([ANO23]{0,1})[SAMP2]*"
#fft.chem[is.na(Species), Site := gsub(sample.name.regex, "\\1", Sample_Name)]
#fft.chem[is.na(Species), Plot := gsub(sample.name.regex, "\\1\\2", Sample_Name)]
#fft.chem[is.na(Species), Age := gsub(sample.name.regex, "\\5", Sample_Name)]
#fft.chem[is.na(Species), Height := gsub(sample.name.regex, "\\4", Sample_Name)]
#fft.chem[is.na(Species), Species := gsub(sample.name.regex, "\\3", Sample_Name)]

#' Merge chemistry data with `fft.infosp`
setkeyv(fft.chem, mergeby.lower)
setkeyv(fft.infosp, mergeby.lower)
fft.dat.raw <- merge(fft.infosp, fft.chem, all=T)

#' Correct column names to match `columns.data`.
oldnames <- c("Sample_Name", "Sample_Year", "Site", "Plot", "Height", "Instrumentation")
newnames <- c("sample_name", "sample_year", "site", "plot", "canopy_position", "instrument")
setnames(fft.dat.raw, oldnames, newnames)

#' Individually fix incorrect columns. Start with descriptive variables...
fft.dat.raw[, project := "FFT"]
fft.dat.raw[, sample_ID := sprintf("%s_%s_%s", project, sample_name, sample_year)]
# MD -- Add it to species info file
fft.dat.raw[,wl.start := 400][,wl.end := 2500]

# TODO: Sort out needle age. Two columns: For pine species, old vs. new. For 
# spruce species, N, 2, 3+
age.pine <- c("N", "O")
age.conifer <- c("N", 2, 3)
pines <- grepl("pine", fft.dat.raw[,PFT])
fft.dat.raw[pines & Age %in% age.pine, pine_needle_oldnew := Age]    # Set pine needle status
fft.dat.raw[!(pines) & Age %in% age.conifer, spruce_needle_age := Age] # Set conifer needle age

#' ...and then move on to quantitative values.
fft.dat.raw[,leaf_water_content := EWT_g_cm2 * 100^2]  # Convert to g m-2
fft.dat.raw[,EWT_g_cm2 := NULL]    # Remove unused column
setnames(fft.dat.raw, "LMA_g_DW_m2", "LMA")    # No conversion
setnames(fft.dat.raw, "Perc_C", "leafC")      # No conversion
setnames(fft.dat.raw, "Perc_N", "leafN")      # No conversion
setnames(fft.dat.raw, "CNRatio", "c2n_leaf")  # No conversion
# No protein
setnames(fft.dat.raw, "CELL_PERC_DW", "leaf_cellulose_percent")
setnames(fft.dat.raw, "ADL_PERC_DW", "leaf_lignin_percent")
# No starch
setnames(fft.dat.raw, "ADF_PERC_DW", "leaf_fiber_percent")
setnames(fft.dat.raw, "SAMPLE_dN15", "leaf_deltaN15")     # TODO: Not sure about this unit

#' Add in remaining information as a long string in "comments" column
fft.dat.raw[, comments := NA]
main.log <- colnames(fft.dat.raw) %in% columns.data
main.cols <- colnames(fft.dat.raw)[main.log]
##comment.cols <- colnames(fft.dat.raw)[!main.log]
##comments <- fft.dat.raw[, paste(comment.cols, sep="__"), with=F)
##fft.dat.raw[, comments := paste(comment.cols, sep="__")]

#' Subset columns to keep.
fft.dat <- fft.dat.raw[, main.cols, with=F]
print.status(fft.dat)

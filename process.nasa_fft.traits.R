source("common.R")

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

#' Perform unit conversions
fft.chem[,leaf_water_content := EWT_g_cm2 * 100^2]  # Convert to g m-2
fft.chem[,EWT_g_cm2 := NULL]    # Remove unused column
setnames(fft.chem, "LMA_g_DW_m2", "leaf_mass_per_area")    # No conversion
setnames(fft.chem, "Perc_C", "leaf_C_percent")      # No conversion
setnames(fft.chem, "Perc_N", "leaf_N_percent")      # No conversion
setnames(fft.chem, "CNRatio", "leaf_CN_ratio")  # No conversion
# No protein
setnames(fft.chem, "CELL_PERC_DW", "leaf_cellulose_percent")
setnames(fft.chem, "ADL_PERC_DW", "leaf_lignin_percent")
# No starch
setnames(fft.chem, "ADF_PERC_DW", "leaf_fiber_percent")
setnames(fft.chem, "SAMPLE_dN15", "leaf_deltaN15")     # TODO: Not sure about this unit

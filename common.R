#' Load packages
library(data.table)
library(reshape2)

source("dbFunctions.R")

db <- src_sqlite("specdb.sqlite")

#' Reflectance and transmittance values are stored as a matrix. The row names 
#' are the sample ID, and the column names are the wavelengths. The following 
#' function facilitates the quick loading of spectra based on sample ID.

get.spec <- function(sample_id, refl.trans="refl", dat=NULL, path='.'){
    if(!is.null(dat)) {
        stopifnot(refl.trans %in% c("refl", "trans"))
        filename <- file.path(path, sprintf("spectra.%s.csv", refl.trans))
        dat <- fread(filename, header=TRUE)
    }
    setkey(dat, "sample_id")
    dat.row <- dat[sample_id][,-1,with=FALSE]
    stopifnot(nrow(dat.row) > 0)
    spec <- as.numeric(dat.row)
    return(spec)
}

#' Small function to check for values in data.table based on column.
#' Mainly used to make sure the sample_id is actually unique.

check.unique <- function(dat, columns="sample_id"){
    dn <- dat[, .N, by=columns]
    dnu <- dn[N > 1]
    nnu <- nrow(dnu)
    if(nnu > 0) stop(sprintf("%d duplicates found", nnu))
}

#' The path for the species information lookup table.
PATH.speciesinfo <- file.path("raw", "species_info.csv")

#' Replace "-999" (and similar) with NA values
replace.na <- function(column, na.val = -999){
    if(is.numeric(column)){
        na.999 <- column <= na.val + 1
        column[na.999] <- NA
    }
    return(column)
}

id_separator <- "|"

#' Define common variables and their units:
columns_traitdata<- c(
        "leaf_nlayers",   # Leaf structure parameter from constrained PROSPECT inversion
        "leaf_chlorophyll_a",      # Chlorophyll a concentration (ug cm-2)
        "leaf_chlorophyll_b",      # Chlorophyll b concentration (ug cm-2)
        "leaf_chlorophyll_total",  # Total Chlorophyll concentration (ug cm-2)
        "leaf_carotenoid_total",   # Total Carotenoid concentraiton (ug cm-2)
        "leaf_anthocyanin_total",  # Total anthocyanin concentration (ug cm-2)
        "leaf_water_content",      # Equivalent water thickness (g m-2)
        "leaf_mass_per_area",      # Leaf dry mass per unit area (g m-2)
        "leaf_C_percent",          # Carbon content (% dry weight)
        "leaf_O_percent",          # Oxygen content (% dry weight)
        "leaf_H_percent",          # Hydrogen content (% dry weight)
        "leaf_N_percent",          # Nitrogen content (% dry weight)
        "leaf_CN_ratio",           # Carbon-Nitrogen ratio (C %DW / N %DW)
        "leaf_protein_percent",    # Protein content (% dry weight)
        "leaf_cellulose_percent",  # Cellulose content (% dry weight)
        "leaf_lignin_percent",     # Lignin content (% dry weight)
        "leaf_starch_percent",     # Starch content (% dry weight)
        "leaf_fiber_percent",      # Fiber content (% dry weight)
        "leaf_deltaN15",           # N15 isotope ratio (ppm)
        "comments"                 # Miscellaneous data
        )

columns_samples <- c('SampleID',
                     'ProjectID',
                     'SpeciesID',
                     'FullName',
                     'SampleName',
                     'SampleYear',
                     'SampleDate',
                     'CanopyPosition',
                     'NeedleAge',
                     'NeedleOldNew',
                     'SiteID',
                     'PlotID',
                     'Comments')

columns_traits <- c('ObservationID',
                    'SampleID',
                    'TraitID',
                    'Value',
                    'Comments')

columns_specInfo <- c('SpectraID',
                      'SpectraName',
                      'SampleID',
                      'SpectraType',
                      'Instrument',
                      'Calibration',
                      'Apparatus',
                      'Comments')

columns_spectra <- c('SpecObsID',
                     'SampleID',
                     'Wavelength',
                     'Value')

columns_sites <- c('SiteID',
                   'SiteName',
                   'SiteDescription',
                   'Comments')

columns_plots <- c('PlotID',
                   'PlotName',
                   'PlotDescription',
                   'Latitude',
                   'Longitude',
                   'Comments')

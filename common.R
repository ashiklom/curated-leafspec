#' Load packages
library(data.table)
library(reshape2)

#' Define common variables and their units:
columns.data <- c(
# Qualitative identifiers
    "project",              # Database from which measurement originated (e.g. LOPEX, ANGERS, FFT)
    "sample_id",            # Sample unique identifier (includes database, sample name and year)
    "sample_name",          # Sample name in original database
    "sample_year",          # Year in which sample was collected
    "species_code",         # Species code (USDA or equivalent)
    "species_scientific",   # Scientific (genus species) name
    "species_common",       # Common species name
    "family",               # Phylogenetic family
    "MD",                   # Monocot or dicot
    "plant_type",           # Plant type -- broadleaf, conifer, shrub, grass, etc.
    "succession",           # Successional stage -- early, mid, late
    "PFT",                  # Functional type, defined as plant type x succession
    "site",                 # Site designation -- currently database-specific
    "plot",                 # Plot designation -- currently database-specific
    "canopy_position",      # Relative vertical canopy position (bottom, middle, top)
    "pine_needle_oldnew",           # Needle age (years) (1, 2, ...) (conifer only)
    "spruce_needle_age",        # Is age greater than 1 year (old) or not (new)?
    "instrument",           # Instrument model for spectral measurement
    "fresh_dry",               # Whether spectrum is on fresh or dry material
    "wl_start", "wl_end",   # First and last wavelength of measurement
# Values
    "leaf_nlayers",   # Leaf structure parameter from constrained PROSPECT inversion
    "leaf_chlorophyll_a",      # Chlorophyll a concentration (ug cm-2)
    "leaf_chlorophyll_b",      # Chlorophyll b concentration (ug cm-2)
    "leaf_chlorophyll_total",  # Total Chlorophyll concentration (ug cm-2)
    "leaf_carotenoid_total",   # Total Carotenoid concentraiton (ug cm-2)
    "leaf_anthocyanin_total",  # Total anthocyanin concentration (ug cm-2)
    "leaf_water_content",      # Equivalent water thickness (g m-2)
    "LMA",                     # Leaf dry mass per unit area (g m-2)
    "leafC",                   # Carbon content (% dry weight)
    "leafO",                   # Oxygen content (% dry weight)
    "leafH",                   # Hydrogen content (% dry weight)
    "leafN",                   # Nitrogen content (% dry weight)
    "c2n_leaf",                # Carbon-Nitrogen ratio (C %DW / N %DW)
    "leaf_protein_percent",   # Protein content (% dry weight)
    "leaf_cellulose_percent", # Cellulose content (% dry weight)
    "leaf_lignin_percent",    # Lignin content (% dry weight)
    "leaf_starch_percent",    # Starch content (% dry weight)
    "leaf_fiber_percent",     # Fiber content (% dry weight)
    "leaf_deltaN15",           # N15 isotope ratio (ppm)
    "comments"                 # Miscellaneous data
    )

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

#' Small function to print which data have and have not been loaded.
print.status <- function(dat){
    present <- columns.data %in% colnames(dat)
    cat("\nThe following data have been loaded:\n")
    print(columns.data[present])
    cat("\nThe following data have NOT been found:\n")
    print(columns.data[!present])
}

#' Replace "-999" (and similar) with NA values
replace.na <- function(column, na.val = -999){
    if(is.numeric(column)){
        na.999 <- column <= na.val + 1
        column[na.999] <- NA
    }
    return(column)
}

id_separator <- "|"


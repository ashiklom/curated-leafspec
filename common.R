#' Load packages
library(data.table)

#' Define common variables and their units:
columns.data <- c(
# Qualitative identifiers
    "database",             # Database from which measurement originated (e.g. LOPEX, ANGERS, FFT)
    "sample_ID",            # Sample unique identifier (includes database, sample name and year)
    "sample_name",          # Sample name in original database
    "sample_year",          # Year in which sample was collected
    "species_code",         # Species code (USDA) or equivalent
    "species_scientific",   # Scientific (genus species) name
    "species_common",       # Common species name
    "MD",                   # Monocot or dicot
    "plant_type",           # Plant type -- broadleaf, conifer, shrub, grass, etc.
    "succession",           # Successional stage -- early, mid, late
    "PFT",                  # Functional type, defined as plant type x succession
    "site",                 # Site designation -- currently database-specific
    "plot",                 # Plot designation -- currently database-specific
    "canopy_position",      # Relative vertical canopy position (bottom, middle, top)
    "needle_age",           # Needle age (years) (1, 2, ...) (conifer only)
    "needle_oldnew",        # Is age greater than 1 year (old) or not (new)?
# Values
    "N",            # Leaf structure parameter from constrained PROSPECT inversion
    "C_a",          # Chlorophyll a concentration (ug cm-2)
    "C_b",          # Chlorophyll b concentration (ug cm-2)
    "C_ab",         # Total Chlorophyll concentration (ug cm-2)
    "C_car",        # Total Carotenoid concentraiton (ug cm-2)
    "C_anth",       # Total anthocyanin concentration (ug cm-2)
    "EWT",          # Equivalent water thickness (cm),
    "LMA",          # Leaf (wet) mass per unit area (g m-2)
    "LDMC",         # Leaf dry mass per unit area (g m-2)
    "LDMC_gcm",     # Leaf dry mass per unit area (g cm-2) (PROSPECT's 'Cm')
    "C_C",          # Carbon content (% dry weight)
    "C_O",          # Oxygen content (% dry weight)
    "C_H",          # Hydrogen content (% dry weight)
    "C_N",          # Nitrogen content (% dry weight)
    "CN_ratio",     # Carbon-Nitrogen ratio (C %DW / N %DW)
    "C_prot",       # Protein content (% dry weight)
    "C_cell",       # Cellulose content (% dry weight)
    "C_lign",       # Lignin content (% dry weight)
    "C_star",       # Starch content (% dry weight)
    "C_fib",        # Fiber content (% dry weight)
    "dN15"          # N15 isotope ratio
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

    

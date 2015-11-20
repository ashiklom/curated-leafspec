#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup

#' Source common script.
source("common.R")

#' Set paths for LOPEX data
PATH.LOPEX <- file.path("raw", "LOPEX")
PATH.chem <- file.path(PATH.LOPEX, "LDB_lopex1993.csv")
PATH.spec <- file.path(PATH.LOPEX, "spec")

#' Load main data.
species.info <- fread(PATH.speciesinfo, header=TRUE)
lopex.chem <- fread(PATH.chem, header=TRUE)

#' Remove spaces from certain names to make it easier to work with.
oldnames <- c("English Name", "Latin Name", "Plant Type")
newnames <- c("common_name", "latin_name", "plant_type_lopex")
setnames(lopex.chem, oldnames, newnames)

#' Assign individual ID to each spectrum and leaf.
lopex.chem[, project := "LOPEX"]
lopex.chem[, sample_year := 1993]

#' Fill in species names and assign ID to each leaf and spectrum.
lopex.chem2 <- lopex.chem
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* .*"
j <- 0
for(i in 1:nrow(lopex.chem2)){
    if(lopex.chem2[i, common_name] == ""){
        lopex.chem2[i, common_name := lopex.chem[i-1, common_name]]
        lopex.chem2[i, latin_name := lopex.chem[i-1, latin_name]]
        k <- k + 1
    } else {
        j <- j + 1
        k <- 1
    }
    lopex.chem2[i, sample_name := sprintf("%s_Leaf%0.2d", 
                                    gsub(species.rxp, "\\1-\\2", latin_name), j)]
    lopex.chem2[i, spec_id := sprintf("%s_Spec%0.2d", sample_name, k)]
}

#' Replace "-999" and "0" with NA values
replace.na <- function(column){
    if(is.numeric(column)){
        na.999 <- column <= -998
        na.zero <- column == 0
        column[na.999 | na.zero] <- NA
    }
    return(column)
}
lopex.chem2 <- lopex.chem2[, lapply(.SD, replace.na)]

#' Set sample ID.
lopex.chem2[, sample_id := sprintf("%s_%s_%s", project, sample_name, sample_year)]

#' Read in reflectance and transmittance data into separate matrices.
lopex.reflspec <- lopex.transspec <- matrix(NA, nrow=nrow(lopex.chem2), ncol=2101)
rownames(lopex.reflspec) <- rownames(lopex.transspec) <- lopex.chem2[,spec_id]
colnames(lopex.reflspec) <- colnames(lopex.transspec) <- as.character(400:2500)
for(i in 1:nrow(lopex.chem2)){
    fname.reflspec <- file.path(PATH.spec, lopex.chem2[i, Refl_file])
    fname.transspec <- file.path(PATH.spec, lopex.chem2[i, Trans_file])
    lopex.reflspec[i,] <- read.table(fname.reflspec, header=FALSE)[,2]
    lopex.transspec[i,] <- read.table(fname.transspec, header=FALSE)[,2]
}

#' Set names for variables that don't need to be changed
oldnames <- c("common_name", "latin_name", "N", "C_a", "C_b", "C_ab", "C_car",
              "C_anth", "C_C", "C_H", "C_O", "C_N") 
newnames <- c("species_common", "species_scientific", "leaf_nlayers", "leaf_chlorophyll_a", 
              "leaf_chlorophyll_b", "leaf_chlorophyll_total", "leaf_carotenoid_total",
              "leaf_anthocyanin_total", "leafC", "leafH", "leafO", "leafN")
setnames(lopex.chem2, oldnames, newnames)

#' Convert necessary units
lopex.chem2[, LMA := LMA * 10000]
lopex.chem2[, leaf_water_content := EWT * 10000]
lopex.chem2[, c2n_leaf := leafC/leafN]
lopex.chem2[, leaf_protein_percent := mean(c(C_prot1, C_prot2), na.rm=TRUE)]
lopex.chem2[, leaf_cellulose_percent := mean(c(C_cell1, C_cell2), na.rm=TRUE)]
lopex.chem2[, leaf_lignin_percent := mean(c(C_lign1, C_lign2), na.rm=TRUE)]
lopex.chem2[, leaf_starch_percent := mean(c(C_star1, C_star2), na.rm=TRUE)]

#' Extract columns that match "common"
matchcols <- colnames(lopex.chem2)[colnames(lopex.chem2) %in% columns.data]
lopex.dat <- lopex.chem2[,matchcols,with=F]
save(lopex.dat, lopex.reflspec, lopex.transspec, file="processed-spec-data/lopex.RData")


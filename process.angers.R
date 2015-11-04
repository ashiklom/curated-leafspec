#' ---
#' title: Process ANGERS data
#' author: Alexey Shiklomanov
#' ---

#' Setup
source("common.R")

#' Set paths
PATH.ANGERS <- file.path("raw", "ANGERS")
PATH.spec <- file.path(PATH.ANGERS, "spec")
PATH.chem <- file.path(PATH.ANGERS, "LDB_angers2003.csv")

#' Load chemistry data.
angers.chem.raw <- fread(PATH.chem, header=TRUE)

#' Remove spaces from certain names to make it easier to work with.
oldnames <- c("English Name", "Latin Name", "Plant Type")
newnames <- c("common_name", "latin_name", "plant_type_lopex")
setnames(angers.chem.raw, oldnames, newnames)

#' Replace "-999" with NA values
replace.na <- function(column){
    if(is.numeric(column)){
        na.999 <- column <= -998
        column[na.999] <- NA
    }
    return(column)
}
angers.chem <- angers.chem.raw[, lapply(.SD, replace.na)]

#' Assign individual ID to each spectrum and leaf.
angers.chem[, project := "ANGERS"]
angers.chem[, sample_year := 2003]
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* .*"
file.rxp <- "an03r(.{4})[.]txt"
angers.chem[, sample_name := sprintf("%s_%s",
                                     gsub(species.rxp, "\\1-\\2", latin_name),
                                     gsub(file.rxp, "\\1", Refl_file))]
angers.chem[, sample_id := sprintf("%s_%s_%s", project, sample_name, sample_year)]

#' Read in reflectance and transmittance data into separate matrices.
angers.reflspec <- angers.transspec <- matrix(NA, nrow=nrow(angers.chem), ncol=2051)
rownames(angers.reflspec) <- rownames(angers.transspec) <- angers.chem[,sample_id]
colnames(angers.reflspec) <- colnames(angers.transspec) <- as.character(400:2450)
for(i in 1:nrow(angers.chem)){
    fname.reflspec <- file.path(PATH.spec, angers.chem[i, Refl_file])
    fname.transspec <- file.path(PATH.spec, angers.chem[i, Trans_file])
    angers.reflspec[i,] <- read.table(fname.reflspec, header=FALSE)[,2]
    angers.transspec[i,] <- read.table(fname.transspec, header=FALSE)[,2]
}

angers.dat <- angers.chem
save(angers.dat, angers.reflspec, angers.transspec, file="angers.RData")

#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---
#' sitename <- "Ispra"
#' sitedesc <- "Joint Research Center, Ispra, Italy"
#' site_lat <- 45.803
#' site_lon <- 8.630
#custom_matches <- c("Lycopersicum esculentum" = 1314,  # Tomato
                    #"Populus x canadensis" = 2808) # Carolina poplar 
#' specinfo_check <- lopex.uniqspec %>%
#'     mutate(Instrument = "Perkin Elmer Lambda 19 double-beam spectrophotometer (BaSO4 integrating sphere)",
#'            Apparatus = "Integrating sphere", 
#'            Calibration = "Spectralon ratio", 
#'            Comments = "See http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral for more info") %>%
#'     mergeWithSQL(db, "specInfo", ., 'SpectraName')

#' # Setup

#' Source common script.
source("common.R")
project_code <- "LOPEX"

#' Set paths for LOPEX data
PATH.LOPEX <- file.path("raw", "LOPEX")
PATH.chem <- file.path(PATH.LOPEX, "LDB_lopex1993.csv")
PATH.spec <- file.path(PATH.LOPEX, "spec")

#' Load main data.
#species.info <- fread(PATH.speciesinfo, header=TRUE)
lopex.chem <- fread(PATH.chem, header=TRUE) %>%
    setnames("Latin Name", "RawSpecies") %>%
    mutate(Project = project_code,
           SampleYear = 1993)

#' Fill in species names and assign ID to each leaf and spectrum.
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* x? ?([[:alpha:]]{3})[[:alpha:]]* *.*"
j <- 0
for(i in 1:nrow(lopex.chem)){
    if(lopex.chem[i, RawSpecies] == ""){
        lopex.chem[i, RawSpecies := lopex.chem[i-1, RawSpecies]]
        k <- k + 1
    } else {
        j <- j + 1
        k <- 1
    }
    lopex.chem[i, SampleName := sprintf("%s_Leaf%0.2d", 
                                    gsub(species.rxp, "\\1-\\2", RawSpecies), j)]
}

#' Replace "-999" and "0" with NA values
lopex.chem <- lopex.chem[, lapply(.SD, replace.na)] %>%
    .[, FullName := paste(Project, SampleName, SampleYear,
                          sep = id_separator)]

#' Read in reflectance and transmittance data
setkey(lopex.chem, FullName)
source("read_spectrum.R")

refl_list <- list()
trans_list <- list()

for (ID in lopex.chem[, unique(FullName)]) {
    refl_files <- lopex.chem[ID, Refl_file]
    refl_files_full <- file.path(PATH.spec, refl_files)
    refl_list[[ID]] <- read_spectrum(refl_files_full)
    trans_files <- lopex.chem[ID, Trans_file]
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- read_spectrum(trans_files_full)
}

lopex.chem.uniq <- lopex.chem[!duplicated(FullName)] %>%
    .[, Reflectance := refl_list[FullName]] %>%
    .[, Transmittance := trans_list[FullName]]

names_dict <- c("N" = "leaf_nlayers",
                "C_a" = "leaf_chlorophyll_a",
                "C_b" = "leaf_chlorophyll_b",
                "C_ab" = "leaf_chlorophyll_total",
                "C_car" = "leaf_carotenoid_total",
                "C_anth" = "leaf_anthocyanin_total",
                "C_C" = "leaf_C_percent",
                "C_H" = "leaf_H_percent",
                "C_O" = "leaf_O_percent",
                "C_N" = "leaf_N_percent")


lopex.traits <- lopex.chem.uniq %>%
    rename_(.dots = setNames(names(names_dict), names_dict)) %>%
    .[, leaf_mass_per_area := LMA * 10000] %>%
    .[, leaf_water_content := EWT * 10000] %>%
    .[, leaf_CN_ratio := leaf_C_percent/leaf_N_percent] %>%
    .[, leaf_protein_percent := 0.5*(C_prot1 + C_prot2)] %>%
    .[, leaf_cellulose_percent := 0.5*(C_cell1 + C_cell2)] %>%
    .[, leaf_lignin_percent := 0.5*(C_lign1 + C_lign2)] %>%
    .[!(is.na(C_star1) | is.na(C_star2)),
        leaf_starch_percent := 0.5*(C_star1 + C_star2)] %>%
    .[(is.na(C_star1) & !is.na(C_star2)),
        leaf_starch_percent := C_star2] %>%
    .[(is.na(C_star2) & !is.na(C_star1)),
        leaf_starch_percent := C_star2] %>%
    subToCols

saveRDS(lopex.traits, file = "processed-spec-data/lopex.rds")

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
#species.info <- fread(PATH.speciesinfo, header=TRUE)
lopex.chem <- fread(PATH.chem, header=TRUE)

#' Remove spaces from certain names to make it easier to work with.
oldnames <- c("English Name", "Latin Name", "Plant Type")
newnames <- c("common_name", "latin_name", "plant_type_lopex")
setnames(lopex.chem, oldnames, newnames)

#' Set global values (see Google sheets)
project_code <- "LOPEX"
project_ID <- getProjectID(project_code)
method_id <- 3

#' Sites table
sitename <- "Ispra"
sitedesc <- "Joint Research Center, Ispra, Italy"
site_lat <- 45.803
site_lon <- 8.630
lopex.site <- tibble(
    SiteName = sitename,
    SiteDescription = sitedesc,
    SiteLatitude = site_lat,
    SiteLongitude = site_lon) %>%
    mergeWithSQL(db, "sites", ., "SiteName")
siteID <- filter(lopex.site, SiteName == sitename) %>% 
    select(SiteID) %>% .[[1]]

#' Assign individual ID to each spectrum and leaf.
lopex.chem <- lopex.chem %>%
    mutate(ProjectName = projectname,
           ProjectID = project_ID,
           sample_year = 1993)

#' Fill in species names and assign ID to each leaf and spectrum.
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* .*"
j <- 0
for(i in 1:nrow(lopex.chem)){
    if(lopex.chem[i, common_name] == ""){
        lopex.chem[i, common_name := lopex.chem[i-1, common_name]]
        lopex.chem[i, latin_name := lopex.chem[i-1, latin_name]]
        k <- k + 1
    } else {
        j <- j + 1
        k <- 1
    }
    lopex.chem[i, sample_name := sprintf("%s_Leaf%0.2d", 
                                    gsub(species.rxp, "\\1-\\2", latin_name), j)]
    lopex.chem[i, spec_id := sprintf("%s_Spec%0.2d", sample_name, k)]
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
lopex.chem <- lopex.chem[, lapply(.SD, replace.na)]

#' Species table
sql_species <- tbl(db, "species") %>%
    distinct(id, scientificname) %>%
    collect() %>%
    rename(SpeciesID = id, ScientificName = scientificname) %>%
    setDT()

custom_matches <- c("Lycopersicum esculentum" = 1314,  # Tomato
                    "Populus x canadensis" = 2808) # Carolina poplar 

lopex.species <- lopex.chem %>%
    mutate(ScientificName = gsub("(.*) L\\..*", "\\1", latin_name)) %>%
    distinct(ScientificName, latin_name) %>%
    left_join(sql_species) %>%
    mutate(SpeciesID = ifelse(is.na(SpeciesID),
                              custom_matches[ScientificName],
                              SpeciesID)) %>%
    filter(!is.na(SpeciesID)) %>%
    setDT() %>%
    setkey(latin_name)

lopex.chem <- lopex.chem %>%
    setkey(latin_name) %>%
    left_join(lopex.species) %>%
    select(-ScientificName, -latin_name, -common_name)

lopex.chem[, FullName := paste(project_code, sample_name, sample_year,
                               sep = id_separator)]

#' Samples table
lopex.samples <- lopex.chem %>%
    rename(SampleYear = sample_year,
           SampleName = sample_name) %>%
    mutate(SiteID = siteID) %>%
    distinct_(.dots = columns_samples[columns_samples %in% colnames(.)]) %>%
    mergeWithSQL(db, "samples", ., "FullName") %>%
    filter(ProjectID == project_ID) %>%
    distinct(SampleID, FullName) %>%
    collect() %>%
    setDT()

#' Spectra info table
vardict <- c("Refl_file" = "Reflectance",
             "Trans_file" = "Transmittance")

lopex.uniqspec <- lopex.chem %>%
    left_join(lopex.samples) %>%
    distinct(SampleID, Refl_file, Trans_file, spec_id) %>%
    melt(id.vars = c('SampleID', 'spec_id')) %>%
    mutate(SpectraType = vardict[variable], 
           SpectraName = paste(project_code,
                               spec_id,
                               substr(variable, 0, 1),
                               sep='_'))

specinfo_check <- lopex.uniqspec %>%
    mutate(Instrument = "Perkin Elmer Lambda 19 double-beam spectrophotometer (BaSO4 integrating sphere)",
           Apparatus = "Integrating sphere", 
           Calibration = "Spectralon ratio", 
           Comments = "See http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm#spectral for more info") %>%
    mergeWithSQL(db, "specInfo", ., 'SpectraName')

lopex.specinfo <- tbl(db, "specInfo") %>%
    filter(SampleID %in% lopex.samples[['SampleID']]) %>%
    distinct(SpectraID, SampleID, SpectraName) %>%
    collect() %>%
    left_join(lopex.uniqspec)

#' Read in reflectance and transmittance data into separate matrices.
message("Reading LOPEX spectra...")
pb <- txtProgressBar(1, nrow(lopex.specinfo), style = 3)
for(i in 1:nrow(lopex.specinfo)){
    spec <- file.path(PATH.spec, lopex.specinfo[['value']][i]) %>%
        fread() %>%
        rename(Wavelength = V1, Value = V2) %>%
        mutate(SpectraID = lopex.specinfo[['SpectraID']][i]) %>%
        mergeWithSQL(db, 'spectra', ., 'SpectraID', return.table = FALSE)
    setTxtProgressBar(pb, i)
}
close(pb)

#' Traits table
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
lopex.traits <- lopex.chem %>%
    rename_(.dots = setNames(names(names_dict), names_dict)) %>%
    left_join(lopex.samples) %>%
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
    select(which(colnames(.) %in% c("SampleID", columns_traitdata))) %>%
    melt(id.vars = 'SampleID', variable.name = "TraitName",
         value.name = "Value") %>%
    left_join(tbl(db, "traitInfo") %>%
              select(TraitID, TraitName) %>%
              collect() %>%
              setDT()) %>%
    select(SampleID, TraitID, Value) %>%
    filter(!is.na(Value)) %>%
    mergeWithSQL(db, 'traits', .)


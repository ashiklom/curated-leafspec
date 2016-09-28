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
newnames <- c("common_name", "latin_name", "plant_type_angers")
setnames(angers.chem.raw, oldnames, newnames)

angers.chem <- angers.chem.raw[, lapply(.SD, replace.na)]

#' Projects table
projectcode <- "ANGERS"
projectname <- "Angers, France spectra from INRA"
angers.project <- tibble(ProjectCode = projectcode,
                         ProjectName = projectname) %>%
    mergeWithSQL(db, "projects", ., "ProjectName")
projectID <- angers.project %>%
    filter(ProjectName == projectname) %>%
    select(ProjectID) %>%
    .[[1]]

#' Sites table
sitename <- "INRA"
sitedesc <- "INRA Centre in Angers, France"
angers.site <- tibble(
    SiteName = sitename,
    SiteDescription = sitedesc) %>%
    mergeWithSQL(db, "sites", ., "SiteName")
siteID <- filter(angers.site, SiteName == sitename) %>% 
    select(SiteID) %>% .[[1]]

angers.plot <- tibble(
    PlotName = sitename,
    PlotDescription = sitedesc,
    SiteID = siteID,
    Latitude = 47.47,
    Longitude = -0.56) %>%
    mergeWithSQL(db, "plots", ., "PlotName")
plotID <- filter(angers.plot, PlotName == sitename) %>%
    select(PlotID) %>% .[[1]]

#' Species table
custom_matches <- c("Acer negundo 'Variegatum'" = 24,  
                    "Calicarpa bodinieri" = 37153,
                    "Cornus alba 'Elegantissima'" = 384,
                    "Corylus maxima 'Purpurea'" = 47278,
                    "Euonymus fortunei 'Emerald'n' Gold'" = 27512,
                    "Euonymus fortunei 'Ovatus Aureus'" = 27512,
                    "Euonymus fortunei 'Silver Queen'" = 27512,
                    "Euonymus fortunei Hand.-Mazz" = 27512,
                    "Hedera helix 'Dentata Variegata'" = 10953,
                    "Hydrangea macrophylla cv" = 25737,
                    "Ilex aquifolium 'Golden Milkboy'" = 31762,
                    "Juglans regia" = 40152,
                    "Prunus laurocerasus 'Otto Luyken'" = 45937,
                    "Rhododendron calophytum" = 18349,
                    "Robinia pseudoacacia 'Frisia'" = 1215,
                    "Salix atrocinerea" = 1246,
                    "Schefflera arboricola 'Gold Capella'" = 44012,
                    "Viburnum plicatum 'Lanarth'" = 21815,
                    "Vitis vinifera L" = 2851,
                    "Weigela florida 'Foliis Purpureis'" = 27891)

sql_species <- tbl(db, "species") %>%
    distinct(id, scientificname) %>%
    collect() %>%
    rename(SpeciesID = id, ScientificName = scientificname) %>%
    setDT()

angers.species <- angers.chem %>%
    mutate(ScientificName = gsub("(.*) L\\..*", "\\1", latin_name)) %>%
    distinct(ScientificName, latin_name) %>%
    left_join(sql_species) %>%
    mutate(SpeciesID = ifelse(is.na(SpeciesID),
                              custom_matches[ScientificName],
                              SpeciesID)) %>%
    filter(!is.na(SpeciesID)) %>%
    setDT() %>%
    setkey(latin_name)

#' Assign individual ID to each spectrum and leaf.
species.rxp <- "([[:alpha:]]{3})[[:alpha:]]* ([[:alpha:]]{3})[[:alpha:]]* .*"
file.rxp <- "an03r(.{4})[.]txt"

oldnames <- c("N",
              "C_a", "C_b",
              "C_ab", "C_car",
              "C_anth") 

newnames <- c("leaf_nlayers",
              "leaf_chlorophyll_a", "leaf_chlorophyll_b",
              "leaf_chlorophyll_total", "leaf_carotenoid_total",
              "leaf_anthocyanin_total")

angers.chem <- angers.chem %>%
    .[, ProjectCode := "ANGERS"] %>%
    .[, ProjectID := projectID] %>%
    .[, SampleYear := 2003] %>%
    .[, SampleName := sprintf("%s_%s",
                               gsub(species.rxp, "\\1-\\2", latin_name),
                               gsub(file.rxp, "\\1", Refl_file))] %>%
    .[, FullName := paste(ProjectCode, SampleName, SampleYear,
                           sep = id_separator)] %>%
    left_join(angers.species) %>%
    select(-ScientificName, -latin_name, -common_name) %>%
    setnames(oldnames, newnames) %>%
    .[, leaf_mass_per_area := LMA * 10000] %>%
    .[, leaf_water_content := EWT * 10000]

#' Samples table
angers.samples <- angers.chem %>%
    .[, PlotID := plotID] %>%
    .[, SiteID := siteID] %>%
    distinct_(.dots = 
                columns_samples[columns_samples %in% colnames(.)]) %>%
    mergeWithSQL(db, "samples", ., "FullName") %>%
    filter(ProjectID == projectID) %>%
    distinct(SampleID, FullName) %>%
    collect() %>%
    setDT()

#' Spectra info table
vardict <- c("Refl_file" = "Reflectance",
             "Trans_file" = "Transmittance")

angers.uniqspec <- angers.chem %>%
    left_join(angers.samples) %>%
    .[, spec_id := gsub(".txt", "", Refl_file)] %>%
    distinct(SampleID, Refl_file, Trans_file, spec_id) %>%
    melt(id.vars = c('SampleID', 'spec_id')) %>%
    mutate(SpectraType = vardict[variable], 
           SpectraName = paste(projectcode,
                               spec_id,
                               substr(variable, 0, 1),
                               sep='_'))

specinfo_check <- angers.uniqspec %>%
    mergeWithSQL(db, "specInfo", ., 'SpectraName')

angers.specinfo <- tbl(db, "specInfo") %>%
    filter(SampleID %in% angers.samples[['SampleID']]) %>%
    distinct(SpectraID, SampleID, SpectraName) %>%
    collect() %>%
    left_join(angers.uniqspec)

#' Read in reflectance and transmittance data into separate matrices.
message("Reading ANGERS spectra...")
pb <- txtProgressBar(1, nrow(angers.specinfo), style = 3)
for(i in 1:nrow(angers.specinfo)){
    spec <- file.path(PATH.spec, angers.specinfo[['value']][i]) %>%
        fread() %>%
        rename(Wavelength = V1, Value = V2) %>%
        mutate(SpectraID = angers.specinfo[['SpectraID']][i]) %>%
        mergeWithSQL(db, 'spectra', ., 'SpectraID', return.table = FALSE)
    setTxtProgressBar(pb, i)
}
close(pb)

#' Traits table
traits_check <- angers.chem %>%
    left_join(angers.samples) %>%
    select_(.dots = c('SampleID', newnames)) %>%
    melt(id.vars = 'SampleID', value.name = "Value", 
         variable.name = "TraitName") %>%
    left_join(tbl(db, "traitInfo") %>% 
              distinct(TraitName, TraitID) %>%
              collect() %>%
              setDT()) %>%
    select(-TraitName) %>%
    setDT() %>%
    filter(!is.na(Value)) %>%
    mergeWithSQL(db, "traits", ., "SampleID", return.table = FALSE)


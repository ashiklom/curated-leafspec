#' ---
#' title: Process ANGERS data
#' author: Alexey Shiklomanov
#' ---
#' 
# projectname <- "Angers, France spectra from INRA"
# project_reference <- "Feret, J.-B., François, C., Asner, G.P., Gitelson, A.A., Martin, R.E., Bidel, L.P.R., Ustin, S.L., le Maire, G., Jacquemoud, S., 2008. PROSPECT-4 and 5: Advances in the leaf optical properties model separating photosynthetic pigments. Remote Sensing of Environment 112, 3030–3043."
# project_doi <- "doi:10.1016/j.rse.2008.02.012"
# project_poc <- "Feret, Jean-Baptiste <feretjb@cesbio.cnes.fr>"
# 
# sitename <- "INRA"
# sitedesc <- "INRA Centre in Angers, France"
# site_lat <- 47.47
# site_lon <- -0.56

# # Species table
# custom_matches <- c("Acer negundo 'Variegatum'" = 24,  
#                     "Calicarpa bodinieri" = 37153,
#                     "Cornus alba 'Elegantissima'" = 384,
#                     "Corylus maxima 'Purpurea'" = 47278,
#                     "Euonymus fortunei 'Emerald'n' Gold'" = 27512,
#                     "Euonymus fortunei 'Ovatus Aureus'" = 27512,
#                     "Euonymus fortunei 'Silver Queen'" = 27512,
#                     "Euonymus fortunei Hand.-Mazz" = 27512,
#                     "Hedera helix 'Dentata Variegata'" = 10953,
#                     "Hydrangea macrophylla cv" = 25737,
#                     "Ilex aquifolium 'Golden Milkboy'" = 31762,
#                     "Juglans regia" = 40152,
#                     "Prunus laurocerasus 'Otto Luyken'" = 45937,
#                     "Rhododendron calophytum" = 18349,
#                     "Robinia pseudoacacia 'Frisia'" = 1215,
#                     "Salix atrocinerea" = 1246,
#                     "Schefflera arboricola 'Gold Capella'" = 44012,
#                     "Viburnum plicatum 'Lanarth'" = 21815,
#                     "Vitis vinifera L" = 2851,
#                     "Weigela florida 'Foliis Purpureis'" = 27891)

#' Setup
source("common.R")
projectcode <- "ANGERS"

#' Set paths
PATH.ANGERS <- file.path("raw", "ANGERS")
PATH.spec <- file.path(PATH.ANGERS, "spec")
PATH.chem <- file.path(PATH.ANGERS, "LDB_angers2003.csv")

#' Load chemistry data.
angers.chem.raw <- fread(PATH.chem, header=TRUE)

setnames(angers.chem.raw, "Latin Name", "RawSpecies")

angers.chem <- angers.chem.raw[, lapply(.SD, replace.na)]

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
    .[, Project := projectcode] %>%
    .[, SampleYear := 2003] %>%
    .[, SampleName := sprintf("%s_%s",
                               gsub(species.rxp, "\\1-\\2", RawSpecies),
                               gsub(file.rxp, "\\1", Refl_file))] %>%
    .[, FullName := paste(Project, SampleName, SampleYear,
                           sep = id_separator)] %>%
    setnames(oldnames, newnames) %>%
    .[, leaf_mass_per_area := LMA * 10000] %>%
    .[, leaf_water_content := EWT * 10000]

#' Read in reflectance and transmittance data into separate matrices.
message("Reading ANGERS spectra...")
setkey(angers.chem, FullName)

refl_list <- list()
trans_list <- list()

for (ID in angers.chem[, unique(FullName)]) {
    refl_files <- angers.chem[ID, Refl_file]
    refl_files_full <- file.path(PATH.spec, refl_files)
    refl_list[[ID]] <- read_spectrum(refl_files_full)
    trans_files <- angers.chem[ID, Trans_file]
    trans_files_full <- file.path(PATH.spec, trans_files)
    trans_list[[ID]] <- read_spectrum(trans_files_full)
}

angers.data <- angers.chem[!duplicated(FullName)] %>%
    .[, Reflectance := refl_list[FullName]] %>%
    .[, Transmittance := trans_list[FullName]] %>%
    subToCols()

saveRDS(angers.data, file = "processed-spec-data/angers.rds")

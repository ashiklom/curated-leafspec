source("common.R")

#' Species table
usda_plants_url <- "http://plants.usda.gov/java/downloadData?fileName=plantlst.txt&static=true"
species <- fread(usda_plants_url) %>%
    setnames(c("USDACode", 
               "USDACodeSynonym", 
               "ScientificName",
               "CommonName",
               "Family"))

cp <- copy_to(db, species, "species", temporary = FALSE,
              unique_indexes = list("SpeciesID"))



#' Google Sheets tables
gs_global <- gs_title("global_tables")

gs_import <- function(table_name) {
    gs_global %>%
        gs_read(ws = table_name) %>%
        copy_to(db, ., table_name, temporary = FALSE)
}

projects <- gs_import("projects")
traitInfo <- gs_import("traitInfo")
instruments <- gs_import("instruments")
specMethods <- gs_import("specMethods")


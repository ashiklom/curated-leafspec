library(googlesheets)
library(dplyr)

gs_main <- gs_title("metadata")
metadata <- gs_read(gs_main, ws = "metadata_columns") %>% 
    select(Column) %>% 
    .[[1]]
writeLines(metadata, "metadata.txt")

traits <- gs_read(gs_main, "traitInfo") %>% 
    select(TraitName) %>% 
    .[[1]]
writeLines(traits, "traits.txt")

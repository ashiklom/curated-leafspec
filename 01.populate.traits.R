library(dplyr)
library(RSQLite)

trait_info <- read.csv("traitInfo.csv", stringsAsFactors = FALSE) %>% 
    tbl_df



library(specprocess)

usda_try <- readRDS('data/common/usda_try.rds')
othersp <- fread('data/common/other_species.csv')
species <- rbind(usda_try, othersp, fill = TRUE)

mrg <- merge_with_sql(species, 'species', by = 'code')

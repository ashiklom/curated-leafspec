# Script for preprocessing the old `species_info.csv` file
library(data.table)

species_path <- file.path("raw/species_info.csv")
species_info <- data.table(read.csv(species_path, stringsAsFactors = F))

names_species <- c("Label" = "FFT_Label",
                   "species_scientific" = "ScientificName",
                   "species_common" = "CommonName",
                   "family" = "Family",
                   "succession" = "Succession")
setnames(species_info, names(names_species), names_species)

# Write FFT-specific conversion file
fft_species <- species_info[, list(FFT_Label, ScientificName)]
write.table(fft_species, file = "raw/NASA_FFT/fft.species.info.csv",
            sep = ",", row.names = FALSE, quote = FALSE)



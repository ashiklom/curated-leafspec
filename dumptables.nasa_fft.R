source("common.R")

dumpdir <- "raw/NASA_FFT/dumps"
dir.create(dumpdir, showWarnings = FALSE)

tabwrite <- function(x, fname, dumpdir = dumpdir)
    fname_full <- file.path(dumpdir, fname)
    write.table(x, file = fname_full, 
                sep = ",", row.names = FALSE, col.names = TRUE)

rawdata <- fread("raw/NASA_FFT/spec/NASA_FFT_LC_Refl_Spectra_v4.csv") %>%
    select(-matches("Wave_")) %>%
    mutate(PlotName = paste(Plot, Sample_Year, sep = "_"))

# Plots table
plots_fft <- fread("raw/NASA_FFT/Plot_Coords.csv") %>%
    rename(Latitude = LAT, Longitude = LON, PlotName = linkplot)

plots <- rawdata %>%
    distinct(Site, Site_Name, State, PlotName) %>%
    left_join(plots_fft)
tabwrite(plots, "plots.csv")

# Sites table
sites <- plots %>%
    group_by(Site, Site_Name, State) %>%
    summarize(Latitude = mean(Latitude, na.rm = TRUE),
              Longitude = mean(Longitude, na.rm = TRUE)) %>%
    .[is.nan(Latitude), Latitude := NA] %>%
    .[is.nan(Longitude), Longitude := NA]
tabwrite(sites, "sites.csv")

# Species table
PATH.fftspecies <- file.path("raw/NASA_FFT/fft.species.info.csv")
species_fftlabel <- fread(PATH.fftspecies) %>% setkey(ScientificName)

species_sql <- tbl(db, "species") %>%
    distinct(id, scientificname) %>%
    collect() %>%
    setDT() %>%
    rename(ScientificName = scientificname)

custom_matches <- c("Carex L." = 251,  # Carex genus
                    "Prunus spp" = 1094) # Prunus genus

species_merge <- left_join(species_fftlabel, species_sql, 
                              by = "ScientificName") %>%
    mutate(SpeciesID = ifelse(is.na(id),
                              custom_matches[ScientificName],
                              id)) %>%
    select(-id) %>%
    filter(!is.na(SpeciesID))

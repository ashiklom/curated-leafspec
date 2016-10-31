source("common.R")
library(readxl)

projectcode <- "ngee_tropics"

path_ngt <- "raw/NGEE-Tropics/"

namesdict <- c("LWP_bar" = "leaf_water_potential",
               "Species" = "RawSpecies", 
               "Location" = "Site")

load_ngt <- function(siteyear) {

    Location <- siteyear[1]
    SampleYear <- siteyear[2]

    path_year <- file.path(path_ngt, paste(Location, SampleYear, sep = "_"))
    chemfile <- list.files(path_year, "Leaf_Samples", full.names = TRUE)
    chemdat <- read_excel(chemfile, sheet = 4) %>%
        setDT() %>%
        setnames(names(namesdict), namesdict) %>%
        .[, SampleYear := SampleYear] %>%
        .[, SampleName := Barcode] %>%
        .[, Project := projectcode] %>%
        .[, FullName := paste(Project, SampleName, SampleYear,
                              sep = id_separator)] %>%
        subToCols()

    specfile <- list.files(path_year, "leaf_spectra",
                            full.names = TRUE)
    specdat <- read_excel(specfile) %>% 
        filter(!(Spectra == "BNL11815" & Site == "PNM")) %>%
        "rownames<-"(., .[['Spectra']]) %>%
        select(-Date, -Spectra, -Site, -Sample_Barcode, -Instrument) %>%
        as.matrix() %>%
        t() %>%
        "/"(100) %>%
        cbind("Wavelength" = as.numeric(gsub('Wave_', '', rownames(.))), .) %>%
        wlmat2list()

    spec_dt <- data.table(SampleName = names(specdat)) %>%
        .[, Reflectance := specdat[SampleName]]

    out <- merge(chemdat, spec_dt, by = 'SampleName', all = TRUE)
    return(out)
}

siteyears <- list(c('Panama', 2016))
ngtdat <- lapply(siteyears, load_ngt) %>% rbindlist() %>% subToCols()
saveRDS(ngtdat, file = rds_name(projectcode))

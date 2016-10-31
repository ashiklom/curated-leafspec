source("common.R")
library(readxl)

projectcode <- "ngee_arctic"

path_nga <- "raw/NGEE-Arctic"

namesdict <- c("C%" = "leaf_C_percent",
               "H%" = "leaf_H_percent",
               "N%" = "leaf_N_percent",
               "LMA ( g m-2)" = "leaf_mass_per_area",
               "C:N" = "leaf_CN_ratio",
               "USDA_Species_Code" = "RawSpecies")

load_nga <- function(SampleYear) {

    path_year <- file.path(path_nga, paste0(SampleYear, "_Data"))
    chemfile <- list.files(path_year, "CHN_LMA", full.names = TRUE)
    chemdat <- read_excel(chemfile, sheet = 2) %>%
        setDT() %>%
        setnames(names(namesdict), namesdict) %>%
        .[, SampleYear := SampleYear] %>%
        .[, SampleName := paste0("BNL", Sample_Barcode)] %>%
        .[, Project := projectcode] %>%
        .[, FullName := paste(Project, SampleName, SampleYear,
                              sep = id_separator)] %>%
        subToCols()

    specfile <- list.files(path_year, "Leaf_GasExchange_Spectra",
                            full.names = TRUE)
    specdat <- read_excel(specfile) %>%
        "rownames<-"(., .[['Spectra']]) %>%
        select(-Date, -Spectra) %>%
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

years <- c(2014)
ngadat <- lapply(years, load_nga) %>% rbindlist() %>% subToCols()
saveRDS(ngadat, file = rds_name(projectcode))

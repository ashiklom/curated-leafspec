# Process Shawn's chlorophyll data
source("common.R")
projectcode <- "arctic_chl"
chl.raw <- fread("raw/Chlorophyll_Data_QA_Spectra_v1_forR.csv", header=TRUE)

chl.raw <- chl.raw[, lapply(.SD, replace.na)] %>%
                .[, Project := projectcode] %>%
                .[, SampleName := Spectra] %>%
                .[, SampleYear := 2012] %>%
                .[, FullName := paste(Project, SampleName, SampleYear,
                                      sep=id_separator)]

# Extract wavelength and other column names
wl.cols <- grepl("Wave_", names(chl.raw))
wl.names <- names(chl.raw)[wl.cols]
other.names <- names(chl.raw)[!wl.cols]
chl.dat <- chl.raw[, other.names, with=F]

# Fix species
setnames(chl.dat, "Species", "Label")
label.dict <- c("SP" = "SP",
                "PF" = "PF",
                "SFP" = "SFP",
                "AF" = "AF",
                "DF" = "DF",
                "CA" = "CA",
                "AL" = "AL",
                "EA" = "EA",
                "VVI" = "VVI")
chl.dat <- chl.dat[, RawSpecies := label.dict[Label]] %>%
                .[, leaf_chlorophyll_a := 
                                Chl_a_g_m2 * 1000 / (100^2)] %>%
                .[, leaf_chlorophyll_b := 
                                Chl_b_g_m2 * 1000 / (100^2)] %>%
                setnames("LMA_gDW_m2", "leaf_mass_per_area")


# Fix other columns
#source("fix.species.R")
#chl.dat <- fix.species(chl.dat)

# Extract reflectance spectra
wavelengths <- gsub("Wave_", "", wl.names) %>% as.numeric()
chl.reflspec <- chl.raw[, wl.names, with=F] %>% 
                as.matrix() %>%
                t() %>%
                "colnames<-"(chl.dat[, FullName]) %>%
                cbind("Wavelength" = wavelengths, .) %>%
                wlmat2list()
chl.dat <- chl.dat[, Reflectance := chl.reflspec[FullName]] %>%
                subToCols()
saveRDS(chl.dat, file = rds_name(projectcode))

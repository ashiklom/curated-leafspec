# Process Shawn's chlorophyll data
source("common.R")
chl.raw <- fread("raw/Chlorophyll_Data_QA_Spectra_v1_forR.csv", header=TRUE)

chl.raw <- chl.raw[, lapply(.SD, replace.na)]
chl.raw[, project := "Arctic_Chl"]
chl.raw[, sample_name := Spectra]
chl.raw[, sample_year := 1000]
chl.raw[, sample_id := paste(project, sample_name, sample_year,
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
chl.dat[, species_scientific := label.dict[Label]]

# Fix other columns
chl.dat[, leaf_chlorophyll_a := Chl_a_g_m2 * 1000 / (100^2)]
chl.dat[, leaf_chlorophyll_b := Chl_b_g_m2 * 1000 / (100^2)]
setnames(chl.dat, "LMA_gDW_m2", "LMA")
print.status(chl.dat)
#source("fix.species.R")
#chl.dat <- fix.species(chl.dat)

# Extract reflectance spectra
chl.reflspec <- chl.raw[, wl.names, with=F]
wl.newnames <- gsub("Wave_", "", wl.names)
setnames(chl.reflspec, wl.names, wl.newnames)
chl.reflspec <- as.matrix(chl.reflspec)
rownames(chl.reflspec) <- chl.raw[, sample_id]

arctic_chl <- list("traits" = chl.dat, "reflectance" = chl.reflspec)
saveRDS(arctic_chl, file = "processed-spec-data/arctic_chl.rds")

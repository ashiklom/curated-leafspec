#` ---
#` title: FFT data processing
#' author: Alexey Shiklomanov
#' ---

#' # Setup

#' Source common script.
source("common.R")

#' Create entry in `projects` table and grab projectID
ProjectCode <- "NASA_FFT"

nasa_fft.projects <- tibble(
    ProjectName = "NASA Forest Functional Types",
    ProjectCode = ProjectCode,
    Affiliation = "FERST - University of Wisconsin Madison",
    PointOfContact = "Serbin, Shawn")

insert_projects <- mergeWithSQL(db, "projects", 
                                nasa_fft.projects,
                                "ProjectName")

projectID <- tbl(db, "projects") %>% 
    filter(ProjectName == nasa_fft.projects[["ProjectName"]]) %>%
    select(ProjectID) %>%
    collect() %>%
    .[[1]]

#' Set paths for FFT data
PATH.FFT <- file.path("raw","NASA_FFT")

#' Get species IDs
PATH.fftspecies <- file.path(PATH.FFT, "fft.species.info.csv")
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


#' # Process reflectance and transmittance data
#' Set reflectance data path
PATH.spec <- file.path(PATH.FFT, "spec")
PATH.refl <- file.path(PATH.spec, "NASA_FFT_LC_Refl_Spectra_v4.csv")

#' Load reflectance data 
nasa_fft.all_refl <- fread(PATH.refl, header=TRUE) %>%
    .[, c("ProjectCode", "ProjectID") := list(ProjectCode, projectID)]

#' Add species ID
nasa_fft.all_refl <- nasa_fft.all_refl %>%
    left_join(rename(species_merge, Species = FFT_Label)) %>%
    select(-Species)

#' Update sites table and get SiteID
sites_merge <- nasa_fft.all_refl %>%
    distinct(Site, Site_Name, State) %>%
    .[, SiteDescription := sprintf("%s (%s); %s, USA",
                                   Site_Name, Site, State)] %>%
    rename(SiteName = Site_Name) %>%
    mergeWithSQL(db, "sites", ., "SiteName") %>%
    setDT()

nasa_fft.all_refl <- nasa_fft.all_refl %>%
    left_join(sites_merge) %>%
    select(-SiteName, -Site, -State)

#' Update plots table and get PlotID
plotcols <- c("Plot", "SiteID")
plots_merge <- nasa_fft.all_refl %>%
    distinct(Plot, SiteID) %>%
    .[, PlotName := sprintf("NASA_FFT Plot %s", Plot)] %>%
    mergeWithSQL(db, "plots", ., "PlotName") %>% 
    select(-SiteID) %>%
    setDT()

nasa_fft.all_refl <- nasa_fft.all_refl %>%
    left_join(plots_merge) %>%
    select(-PlotName, Plot) 

#' Samples table
                                      
#' Create FullName as database + sample_name + year
nasa_fft.all_refl <- nasa_fft.all_refl %>%
    .[, FullName := paste(ProjectCode, Sample_Name, Sample_Year,
                             sep = id_separator)] %>%
    .[, SpectraName := paste(FullName, "R", sep = "_")]
check.unique(nasa_fft.all_refl, "FullName")

#' Isolate full sample information
all.cols <- colnames(nasa_fft.all_refl)
wl.cols <- sprintf("Wave_%d", 350:2500)
info.cols <- all.cols[!(all.cols %in% wl.cols)]
nasa_fft.all <- nasa_fft.all_refl[,info.cols, with=F]

#' Correct column names
names_samples <- c("Sample_Name" = "SampleName",
                   "Sample_Year" = "SampleYear",
                   "Height" = "CanopyPosition")

names_specInfo <- c("Instrumentation" = "Instrument",
                    "Measurement_Type" = "Apparatus",
                    "Measurement" = "SpectraType")

names_all <- c(names_samples, names_specInfo)

setnames(nasa_fft.all, names(names_all), names_all)

#' Sort out needle age variables
fixNeedleAge <- function(dat) {
    age.pine <- c("N", "O")
    age.conifer <- c("N", 2, 3)
    out <- dat %>%
        mutate(NeedleOldNew = ifelse(Age %in% age.pine, Age, NA),
            NeedleAge = ifelse(Age %in% age.conifer, Age, NA)) %>%
        mutate(NeedleAge = ifelse(NeedleAge == "N", 1, NeedleAge)) %>%
        mutate(NeedleAge = as.numeric(NeedleAge))
    return(out)
}

nasa_fft.all <- fixNeedleAge(nasa_fft.all)

#' Isolate samples table
samples_merge <- nasa_fft.all %>%
    select(which(colnames(.) %in% columns_samples)) %>%
    mergeWithSQL(db, "samples", ., "FullName") %>%
    select(SampleID, FullName)

nasa_fft.all <- left_join(nasa_fft.all, samples_merge)

#' Build SpecInfo table
specinfo_merge <- nasa_fft.all %>%
    select(which(colnames(.) %in% columns_specInfo)) %>%
    mutate(Apparatus = ifelse(Apparatus == "LC",
                              "Leaf clip",
                              Apparatus),
           SpectraType = ifelse(SpectraType == "REFL",
                                "Reflectance",
                                SpectraType)) %>%
    mergeWithSQL(db, "specInfo", ., "SpectraName") %>%
    select(SpectraID, SampleID)
 
#' Pull out reflectance spectra into its own data.table
refl_check <- nasa_fft.all_refl %>%
    left_join(samples_merge) %>%
    left_join(specinfo_merge) %>%
    select_(.dots = c("SpectraID", wl.cols)) %>%
    melt(id.vars = "SpectraID", value.name = "Value") %>%
    mutate(Wavelength = as.numeric(sub("Wave_", "", variable))) %>%
    select(-variable) %>%
    mergeWithSQL(db, "spectra", . , 'SpectraID')

#' Repeat above steps for transmittance
PATH.trans <- file.path(PATH.spec, "NASA_FFT_IS_Tran_Spectra_v4.csv")
nasa_fft.trans_all <- fread(PATH.trans, header=TRUE) %>%
    .[, FullName := paste(ProjectCode, Sample_Name, Sample_Year, 
                              sep = id_separator),] %>%
    .[, ProjectID := projectID] %>%
    .[, SpectraName := paste(FullName, "T", sep = "_")] %>%
    setnames(names(names_all), names_all)

fullnames_present <- tbl(db, "samples") %>%
    distinct(FullName) %>%
    collect() %>%
    .[[1]]

nasa_fft.samples_trans <- nasa_fft.trans_all %>%
    filter(!FullName %in% fullnames_present) %>%
    left_join(sites_merge) %>%
    left_join(plots_merge) %>%
    left_join(rename(species_merge, Species = FFT_Label)) %>%
    fixNeedleAge() %>%
    select(which(colnames(.) %in% columns_samples))

samples_trans_merge <- nasa_fft.samples_trans %>%
    mergeWithSQL(db, "samples", ., "FullName") %>%
    select(SampleID, FullName)

nasa_fft.trans_all <- nasa_fft.trans_all %>%
    left_join(samples_trans_merge)

nasa_fft.specinfo_trans <- nasa_fft.trans_all %>%
    select(which(colnames(.) %in% columns_specInfo)) %>%
    mutate(Apparatus = ifelse(Apparatus == "IS", 
                              "Integrating sphere", 
                              Apparatus),
           SpectraType = ifelse(SpectraType == "TRAN",
                                "Transmittance",
                                SpectraType))

specinfo_trans_merge <- nasa_fft.specinfo_trans %>%
    mergeWithSQL(db, "specInfo", ., 'SpectraName') %>%
    select(SpectraID, SampleID, SpectraName)

nasa_fft.trans <- nasa_fft.trans_all %>%
    left_join(samples_merge) %>%
    left_join(specinfo_merge) %>%
    select_(.dots = c("SpectraID", wl.cols)) %>%
    melt(id.vars = "SpectraID", value.name = "Value") %>%
    mutate(Wavelength = as.numeric(sub("Wave_", "", variable))) %>%
    select(-variable)

trans_check <- mergeWithSQL(db, "spectra", nasa_fft.trans,
                           return.table = FALSE)

#' Process trait data
source("process.nasa_fft.traits.R")
nasa_fft.traits <- fft.chem %>%
    left_join(tbl(db, "samples") %>% 
              filter(ProjectID == projectID) %>%
              select(SampleID,
                     Sample_Name = SampleName, 
                     Sample_Year = SampleYear) %>%
              collect() %>%
              setDT()) %>%
    select(-Sample_Name, -Sample_Year) %>%
    melt(id.vars = "SampleID", value.name = "Value") %>%
    filter(!is.na(Value), !is.na(SampleID)) %>%
    left_join(tbl(db, "traitInfo") %>%
              select(TraitID, variable = TraitName) %>%
              collect() %>%
              setDT()) %>%
    select(SampleID, TraitID, Value) %>%
    filter(!is.na(Value)) %>%
    mergeWithSQL(db, "traits", ., return.table = FALSE)
 

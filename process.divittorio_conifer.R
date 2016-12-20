source("common.R")

projectcode <- "divittorio_conifer"
projpath <- "raw/divittorio_conifer/"

# Get all spec file names
# Commented-out directories are redundant ones
include_dirs <- c('sgf_oz',
#                  'sgf_oz1',
#                  'sgf_oz2',
                  'sgf_gr',
#                  'sgf_gr1',
#                  'sgf_gr2',
                  'sgf_wf',
#                  'sgf_wf1',
#                  'sgf_wf2',
                  'sqa_t1',
                  'sqa_t2',
                  'sqa_t3',
                  'sqa_t4',
                  'sqa_gr',
                  'sqa_wf',
                  'sqa_scale',
                  'sqa_suck',
                  'qcy_gr',
                  'qcy_wf',
                  'qcy_t1',
                  'qcy_t2',
                  'qcy_t3',
                  'qcy_t4')

namesdict <- c("chla" = "leaf_chlorophyll_a",
               "chlb" = "leaf_chlorophyll_b",
               "tchl" = "leaf_chlorophyll_total",
               "carot" = "leaf_carotenoid_total")

site_spec <- c("qcy" = "Plumas National Forest",
               "sgf" = "Giant Forest",
               "sqa" = "Mineral King")

site_chem <- c("pnf" = "Plumas National Forest",
               "gf" = "Giant Forest",
               "mk" = "Mineral King")

cond <- c("oz" = "ozone",
          "oz1" = "ozone",
          "oz2" = "ozone",
          "gr" = "green",
          "wf" = "winter_fleck",
          "scale" = "scale_insect",
          "suck" = "sucking_insect",
          "rand" = "random",
          "t1" = "1", 
          "t2" = "2",
          "t3" = "3",
          "t4" = "4")


# NOTE: Only using the top spectra 
specfiles <- list.files(file.path(projpath, include_dirs), 
                        pattern = "_top_.*.txt", 
                        full.names = TRUE) %>%
    grep("_[12]d_", ., value = TRUE, invert = TRUE)

specregex <- '(^[[:alpha:]]+)_([[:alnum:]]+)_top_s([[:digit:]]+$)'

specdat <- data.table(Project = projectcode,
                      specfile = specfiles,
                      SampleYear = 2009) %>%
    .[, rawstring := gsub('.txt', '', basename(specfile))] %>%
    .[, rawsite_spec := gsub(specregex, "\\1", rawstring)] %>%
    .[, rawcond_spec := gsub(specregex, "\\2", rawstring)] %>%
    .[, specnum := as.numeric(gsub(specregex, "\\3", rawstring))] %>%
    .[, Site := site_spec[rawsite_spec]] %>%
    .[, OtherCondition := cond[rawcond_spec]] %>%
    .[, treenum := as.numeric(cond)] %>%
    .[OtherCondition == 'ozone1', treenum := 1] %>%
    .[OtherCondition == 'ozone2', treenum := 2] %>%
    .[OtherCondition %in% c('ozone1', 'ozone2'), OtherCondition := 'ozone'] %>%
    .[OtherCondition %in% c('1','2','3','4'), OtherCondition := 'random']


chempath <- file.path(projpath, "all_pig.txt")

chemraw <- read.table(chempath, header = TRUE,
                      stringsAsFactors = FALSE)

head_inds <- as.numeric(chemraw[,1]) %>% is.na() %>% which

chemlist <- list()

for (i in seq_along(head_inds[-1])) {
    h <- head_inds[i]
    a <- head_inds[i] + 1
    b <- head_inds[i+1] - 1
    info <- unique(unlist(chemraw[h,]))
    stopifnot(length(info) == 1)
    dat <- chemraw[a:b,] %>% 
        lapply(as.numeric) %>%
        as.data.table %>%
        .[, info := info]
    chemlist[[i]] <- dat
}

chemdat <- rbindlist(chemlist) %>%
    setnames(names(namesdict), namesdict) %>%
    .[, rawsite := gsub("(.*)-(.*)", "\\1", info)] %>%
    .[, rawcond := gsub("(.*)-(.*)", "\\2", info)] %>%
    .[, Site := site_chem[rawsite]] %>%
    .[, OtherCondition := cond[rawcond]] %>%
    .[, specnum := 1:.N, by = info] %>%
    .[Site == "Mineral King" & OtherCondition == "random",
      treenum := rep(1:4, each = 32)] %>%
    .[rawcond == 'oz1', OtherCondition := '

# Break up merge by site -- this seems to work better
gf_chem <- chemdat[Site == 'Giant Forest']
gf_spec <- specdat[Site == 'Giant Forest']
giant_forest <- data.table(Site = 'Giant Forest',
                           OtherCondition = gf_chem[, OtherCondition],
                           specnum = gf_chem[, specnum],
                           treenum = gf_chem[, treenum],
                           leaf_chlorophyll_a = gf_chem[, leaf_chlorophyll_a],
                           leaf_chlorophyll_b = gf_chem[, leaf_chlorophyll_b], 
                           leaf_chlorophyll_total = gf_chem[, leaf_chlorophyll_total],
                           leaf_carotenoid_total = gf_chem[, leaf_carotenoid_total], 
                           specfile = gf_spec[, specfile])

mk_chem <- chemdat[Site == 'Mineral King']
mk_spec <- specdat[Site == 'Mineral King']
mineral_king <- data.table(Site = 'Mineral King',
                           OtherCondition = mk_chem[, OtherCondition],
                           specnum = mk_chem[, specnum],
                           treenum = mk_chem[, treenum],
                           leaf_chlorophyll_a = mk_chem[, leaf_chlorophyll_a],
                           leaf_chlorophyll_b = mk_chem[, leaf_chlorophyll_b], 
                           leaf_chlorophyll_total = mk_chem[, leaf_chlorophyll_total],
                           leaf_carotenoid_total = mk_chem[, leaf_carotenoid_total], 
                           specfile = mk_spec[, specfile])

plumas_chem <- chemdat[Site == 'Plumas National Forest']
plumas_spec <- specdat[Site == 'Plumas National Forest']
plumas <- data.table( OtherCondition = plumas_chem_sub[, OtherCondition],
                     specnum = plumas_chem[, specnum],
                     treenum = plumas_chem[, treenum],
                     leaf_chlorophyll_a = plumas_chem[, leaf_chlorophyll_a],
                     leaf_chlorophyll_b = plumas_chem[, leaf_chlorophyll_b], 
                     leaf_chlorophyll_total = plumas_chem[, leaf_chlorophyll_total],
                     leaf_carotenoid_total = plumas_chem[, leaf_carotenoid_total], 
                     specfile = plumas_spec[OtherCondition != 'random', specfile])
plumas <- rbindlist(list(plumas, plumas_spec[OtherCondition == 'random', 
                         .(specfile, specnum, OtherCondition, treenum)]),
                    use.names = TRUE, fill = TRUE) %>%
    .[, Site := 'Plumas National Forest']

plumas[, .N, is.na(specfile)]

divdat <- rbindlist(list(giant_forest, mineral_king, plumas), use.names = TRUE)
divdat[, .N, is.na(specfile)]
divdat[, .N, specfile][N > 1]

# Load spectra as a list
refl_list <- trans_list <- list()
for (i in seq_len(nrow(divdat))) {
    specfile <- divdat[i, specfile]
    spec <- fread(specfile)
    setnames(spec, c('Wavelength', 'Reflectance', 'Transmittance'))
    # Downsample to PROSPECT resolution
    minx <- ceiling(min(spec[, Wavelength]))
    maxx <- floor(max(spec[, Wavelength]))
    newx <- minx:maxx
    refl_list[[i]] <- spec[, .(Wavelength, Reflectance)] %>%
        spline(x = .[,Wavelength], y = .[,Reflectance], xout = newx) %>%
        do.call(cbind, .) %>%
        'colnames<-'(c('Wavelength', 'Reflectance')) %>%
        specobs
    trans_list[[i]] <- spec[, .(Wavelength, Transmittance)] %>%
        spline(x = .[,Wavelength], y = .[,Transmittance], xout = newx) %>%
        do.call(cbind, .) %>%
        'colnames<-'(c('Wavelength', 'Transmittance')) %>%
        specobs
}


divdat <- divdat %>%
    .[, Reflectance := refl_list] %>%
    .[, Transmittance := trans_list] %>%
    .[, SampleName := paste(substr(Site, 0, 1), OtherCondition, specnum, treenum,
                            sep = '_')]
divdat[, .N, .(Site, SampleName)][N > 1]

# Set unique sample name everywhere
divdat <- divdat[

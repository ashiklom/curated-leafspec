source("common.R")
projectcode <- "divittorio_conifer"

projpath <- "raw/divittorio_conifer/"

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

namesdict <- c("chla" = "leaf_chlorophyll_a",
               "chlb" = "leaf_chlorophyll_b",
               "tchl" = "leaf_chlorophyll_total",
               "carot" = "leaf_carotenoid_total")

site_chem <- c("gf" = "Giant Forest",
               "mk" = "Mineral King",
               "pnf" = "Plumas National Forest")


conditions <- c("oz" = "ozone",
                "gr" = "green",
                "wf" = "winter_fleck",
                "scale" = "scale_insect",
                "suck" = "sucking_insect",
                "rand" = "random")

site_spec <- c("Plumas National Forest" = "qcy",
              "Giant Forest" = "sgf",
              "Mineral King" = "sqa")

chemdat <- rbindlist(chemlist) %>%
    setnames(names(namesdict), namesdict) %>%
    .[, rawsite := gsub("(.*)-(.*)", "\\1", info)] %>%
    .[, rawcond := gsub("(.*)-(.*)", "\\2", info)] %>%
    .[, Site := site_chem[rawsite]] %>%
    .[, OtherCondition := conditions[rawcond]] %>%
    .[, Project := projectcode] %>%
    .[, SampleYear := 2009] %>%
    .[, specnum := 1:.N, by = info] %>%
    .[, SampleName := paste(info, specnum, sep = "_")] %>%
    .[, FullName := paste(Project, SampleName, SampleYear, 
                          sep = id_separator)] %>%
    .[, specprefix := sprintf("%1$s/%1$s_(top|bot)_s%2$d",
                              paste(site_spec[Site],
                                    rawcond, 
                                    sep = "_"),
                              specnum)] %>%
    .[OtherCondition == "random",
      specprefix := 


specdirs <- list.dirs(projpath, recursive = FALSE)

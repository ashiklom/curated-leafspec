read_spectrum <- function(filenames) {
    wlmerge <- function(d1, d2) merge(d1, d2, by = "Wavelength")
    lapply(refl_files_full, read.table, header = FALSE) %>%
        lapply(setnames, c("Wavelength", "Value")) %>%
        Reduce(wlmerge, .) %>%
        as.matrix() %>% 
        "colnames<-"(c("Wavelength", refl_files)) %>%
        specobs()
}

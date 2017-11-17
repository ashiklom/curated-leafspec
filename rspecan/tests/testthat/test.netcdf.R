library(rspecan)
library(testthat)
context("Reading and writing NetCDF spectra files")

specfile <- "testspec.nc"

suppressWarnings({
  .zzz <- file.remove(specfile)
  .zzz <- rm("nc")
})

## Create file
create_specfile(specfile)

data(testspec, package = "PEcAnRTM")
mat <- testspec_ACRU[, 1:2]
wavelengths <- seq(400, 2500)

add <- add_spectra(testspec_ACRU[, 1:10], specfile)

# Try retrieving values
ind <- c(6, 4, 2)
test_that("Retrieved spectra match input spectra", {
            expect_equivalent(get_spectra(ind, specfile), testspec_ACRU[, ind])
})

.zzz <- file.remove(specfile)

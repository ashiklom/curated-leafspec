specfile <- "tests/testspec.nc"

suppressWarnings({
  .zss <- file.remove(specfile)
  .zzz <- rm("nc")
})

## Create file
create_specfile(specfile)

data(testspec, package = "PEcAnRTM")
mat <- testspec_ACRU[, 1:2]
wl <- seq(400, 2500)
filename <- specfile

add_spectra(testspec_ACRU[, 1:2], specfile, start_id = 1)
add_spectra(testspec_ACRU[, 3:4], specfile)

# Test that values are retrievable
nc <- ncdf4::nc_open(specfile)
out <- ncdf4::ncvar_get(nc, "spectra")
dim(out)

file.remove(specfile)

library(rspecan)
library(testthat)

test_settings <- list(
  init = list(
    iterations = 2500
  ),
  loop = list(
    iterations = 1000
  ),
  other = list(
    threshold = 1.5,
    min_samp = 500
  )
)

samps1 <- run_inversion("ecosis_01", "h7bacfa1",
                        custom_settings = test_settings)
process_samples(samps1)

#' @export
run_inversion <- function(data_name, spectra_id, prospect_version = "D", ...) {
  data_path <- here::here("processed_data", paste0(data_name, ".rds"))
  stopifnot(file.exists(data_path))

  datalist <- readRDS(data_path)
  observed <- datalist$spectra[datalist$data_wl_inds, spectra_id]
  metadata <- datalist$metadata %>%
    filter(spectra_id == !!spectra_id)
  stopifnot(nrow(metadata) == 1)

  if (metadata$spectra_type == "reflectance") {
    rtm <- function(param) {
      prospect(param, prospect_version)[, 1]
    }
  } else if (metadata$spectra_type == "pseudo-absorbance") {
    rtm <- function(param) {
      pout <- prospect(param, prospect_version)[, 1]
      log10(1 / pout)
    }
  } else {
    stop("Unknown spectra type \"", metadata$spectra_type, "\"")
  }

  model <- function(params) rtm(params)[datalist$prospect_wl_inds]
  prior <- prospect_bt_prior(prospect_version)
  test_mod <- model(defparam(paste0("prospect_", tolower(prospect_version))))
  stopifnot(length(test_mod) == length(observed))
  invert_bt(
    observed = observed,
    model = model,
    prior = prior,
    ...
  )
}

#' @export
process_samples <- function(samps) {
  samps_mcmc <- BayesianTools::getSample(samps, coda = TRUE)
  samps_burned <- PEcAn.assim.batch::autoburnin(samps_mcmc, method = "gelman.plot")
  samps_summary <- summary(samps_burned)
  bound <- cbind(samps_summary$statistics, samps_summary$quantiles)
  summary_df <- as_tibble(bound) %>%
    mutate(parameter = rownames(bound)) %>%
    select(parameter, everything())
  list(
    summary_df = summary_df,
    samples = samps_burned
  )
}

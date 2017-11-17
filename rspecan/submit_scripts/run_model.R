library(rspecan)
sethere()
message("Current working directory: ", getwd())

results_dir <- "raw_results"
dir.create(results_dir, showWarnings = FALSE)

arg <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arg) == 2)

submit_data_file <- arg[1]
spnum <- as.integer(arg[2])
stopifnot(
  file.exists(submit_data_file),
  is.integer(spnum)
)

submit_data <- read_csv(submit_data_file)[spnum, ]
data_name <- submit_data[["data_name"]]
spectra_id <- submit_data[["spectra_id"]]

results_fname <- file.path(results_dir, paste(data_name, spectra_id, "rds"))

message("Dataset: ", data_name)
message("Spectra ID: ", spectra_id)

message("Starting inversion...")
samples <- run_inversion(submit_data$data_name, submit_data$spectra_id)

message("Processing samples...")
result <- process_samples(samples)

message("Saving results to: ", results_fname)
saveRDS(result, results_fname)
message("Done!")

#' @export
write_submit_file <- function(submit_df,
                              jobname = unique(submit_df$data_name)[1]
                              ) {
  nsubmit <- nrow(submit_df)
  sge_header <- c(
    "#!/bin/bash -l",
    "#$ -j y",
    paste0("#$ -o ", log_dir, "/"),
    "#$ -q \"geo*\"",
    "#$ -l h_rt=11:59:00",
    paste("#$ -N", jobname),
    sprintf("#$ -t 1-%d", nsubmit),
    ""
  )
  submit_data_file <- file.path(submit_dir, paste0(jobname, ".submit"))
  submit_string <- sprintf(
    "Rscript run_model.R %s $SGE_TASK_ID",
    submit_data_file
  )
  submit_script <- c(sge_header, submit_string)
  submit_scriptfile <- file.path(submit_dir, jobname)

  write_csv(submit_df, submit_data_file)
  writeLines(submit_script, submit_scriptfile)
  Sys.chmod(submit_scriptfile, mode = "0777")
}

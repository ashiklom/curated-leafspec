#' Create RDS name from project code
#'
#' @export
rds_name <- function(projectcode, directory = 'processed-spec-data') {
    sprintf("%s/%s.rds", directory, projectcode)
}

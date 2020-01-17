#' project_annual_throughputs_by
#'
#' @export
project_annual_throughputs_by <- function (
  ...,
  verbose = getOption("verbose")
) {

  project_annual_quantities_by(
    ...,
    value_vars = "tput_qty",
    growth_var = "gf_qty",
    verbose = verbose)

}

#' project_annual_throughputs
#'
#' @noRd
#'
#' @export
project_annual_throughputs <-
  project_annual_throughputs_by

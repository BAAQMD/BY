project_annual_quantities_by <- function (
  input_data,
  ...,
  using,
  value_vars,
  growth_var,
  drop = TRUE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[project_annual_quantities_by] ", ...)

  by_vars <-
    as.character(eval(substitute(alist(...))))

  msg("value_vars is: ", str_csv(value_vars))
  msg("growth_var is: ", growth_var)
  msg("by_vars is: ", str_csv(by_vars))

  stopifnot(
    all_same(pull(input_data, year)))

  base_year_data <-
    input_data %>%
    sum_annual_quantities_by(
      ...) %>%
    drop_vars(
      year)

  projected_data <-
    inner_join(
      base_year_data,
      using,
      by = by_vars) %>%
    rename(
      gf_qty = !!growth_var) %>%
    mutate_at(
      vars(value_vars),
      ~ . * gf_qty) %>%
    dplyr::select(
      names(input_data),
      gf_qty)

  if (isTRUE(drop)) {

    projected_data <-
      dplyr::select(
        projected_data,
        -gf_qty)

  } else {

    projected_data <-
      rename(
        projected_data,
        !!growth_var := gf_qty)

  }

  msg("projected_data has ", nrow(projected_data), " rows")

  return(projected_data)

}

#' project_annual_quantities
#'
#' @noRd
#' @export
project_annual_quantities <-
  project_annual_quantities_by

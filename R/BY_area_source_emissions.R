#' @include BY_area_source_projections.R

#' BY_area_source_emissions
#'
#' @description `BY_area_source_emissions()` returns results for a single year (the base year).
#'
#' @rdname BY_area_source_projections
#' @usage BY_area_source_emissions(...)
#'
#' @export
BY_area_source_emissions <- function (
  base_year,
  ...,
  verbose = getOption("verbose")
) {

  BY_area_source_emission_data <-
    BY_area_source_projections(
      base_year,
      years = CY(elide_year(base_year)),
      ...,
      verbose = verbose)

  return(BY_area_source_emission_data)

}

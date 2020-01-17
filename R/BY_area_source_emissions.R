#' @export
BY_area_source_emissions <- function (
  base_year,
  ...,
  verbose = getOption("verbose")
) {

  BY_area_source_emission_data <-
    base_year %>%
    BY_area_source_projections(
      years = CY(elide_year(base_year)),
      ...,
      verbose = verbose)

  return(BY_area_source_emission_data)

}

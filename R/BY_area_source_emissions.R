#' @export
BY_area_source_emissions <- function (
  base_year,
  ...,
  verbose = getOption("verbose")
) {

  yyyy <-
    as.integer(base_year)

  BY_area_source_projection_data <-
    base_year %>%
    BY_area_source_projections(
      years = yyyy,
      ...,
      verbose = verbose)

  single_year_data <-
    BY_area_source_projection_data %>%
    filter_years(
      yyyy)

  return(single_year_data)

}

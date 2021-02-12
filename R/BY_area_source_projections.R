#' @name BY_area_source_projections
#'
#' @title
#' Calculate, Forecast, and Backcast Emissions for Area Sources
#'
#' @description
#' `BY_area_source_projections()` combines throughputs, emission factors, control
#' factors, and growth profiles, multiplying them together. This results in a
#' set of "grown and controlled" emissions for the given `years`.
#'
#' @return
#' Tabular data:
#' - keyed by year, county, pollutant, and category;
#' - having columns `tput_qty`, `ef_qty`, `cf_qty`, `gf_qty`, and their product, `ems_qty`.
#'
#' @param base_year `BY` object, like `BY(2011)`
#' @param years `CY` object, like `CY(1990:2050)`; the range of years you want results for
#' @param tput_data see Details, below
#' @param ef_data see Details, below
#' @param cf_data see Details, below
#' @param gpf_data see Details, below
#' @param na.rm passed to subroutines; refer to "See Also", below
#' @param verbose display informative messages
#'
#' @details
#'
#' **BASIC USAGE**
#'
#' If you don't supply any or all of:
#'
#'   - `tput_data`
#'   - `ef_data`
#'   - `cf_data`
#'   - `gpf_data`
#'
#' ... then, helpfully, `BY_area_source_projections()` will fill them in for you.
#' In that case, so that it knows where to look, you will have to supply a base
#' year, like so:
#'
#' ```
#'     BY(2011) %>% BY_area_source_projections()
#' ```
#'
#' In this case, it's helpful to pass `verbose = TRUE`, so that you'll know
#' what's happening and where the throughput, emission factor, control factor,
#' and growth profile data are being pulled from.
#'
#' **ADVANCED USAGE**
#'
#' You can, instead, BYOD ("bring your own data"). Then you can use this function to
#' explore combinations of alternate throughputs, growth factors, emission
#' factors, and control factors.
#'
#' You can simply read in a CSV file, or an XLSX worksheet, containing the
#' `tput_data`, `ef_data`, `cf_data`, or `gpf_data` that you would like to use.
#' (You will have to use `read_csv()` or `read_excel()` to import the data
#' first.)
#'
#' The data that you do supply should look like the defaults:
#'
#' - `tput_data` defaults to `DB_area_source_throughputs(base_year)`.
#' - `ef_data` defaults to `annualize_emission_factors(DB_area_source_emission_factors(base_year))`.
#' - `cf_data` defaults to `annualize_control_factors(DB_area_source_control_factors(base_year))`.
#' - `gpf_data` defaults to `DB_growth_profiles(base_year))`.
#'
#' Try out those defaults, or look at the help pages. (Click the links under "See Also", below.)
#' As long as you supply similar-looking data, you are good to go.
#'
#' @note If you don't supply `tput_data`, then data will be fetched, and results
#'   will be calculated, for every single category in the given base year. If
#'   you do supply `tput_data`, the result will be scoped to just the categories
#'   present in `tput_data$cat_id`. (This will be noticeably faster if the number of categories is small.)
#'
#' @seealso
#' - [DB_area_source_throughputs()]
#' - [DB_area_source_emission_factors()] and [annualize_emission_factors()]
#' - [DB_area_source_control_factors()] and [annualize_control_factors()]
#' - [DB_growth_profiles()]
#'
NULL

#' @name BY_area_source_projections
#' @usage NULL
#'
BY_area_source_projections_ <- function (
  base_year = NULL,
  years = CY(1990:2040),
  tput_data = NULL,
  ef_data = NULL,
  cf_data = NULL,
  gf_data = NULL,
  na.rm = TRUE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[BY_area_source_projections] ", ...)

  #
  # If `tput_data` is NULL, then attempt to fetch it from DataBank.
  #
  if (is.null(tput_data)) {
    msg("tput_data <- DB_area_source_throughputs()")
    tput_data <-
      DB_area_source_throughputs(
        base_year = base_year,
        na.rm = na.rm,
        verbose = verbose)
  }

  #
  # Construct `projected_tput_data`.
  #
  # If `gf_data` is NULL, and `base_year` is also NULL,
  # then this will just be identical to `tput_data`,
  # but with a new column `gf_qty` set equal to 1.
  #
  if (is.null(gf_data) && is.null(base_year)) {

    projected_tput_data <-
      mutate(
        tput_data,
        gf_qty = 1.00)

  } else {

    #
    # We have a `base_year`, but `gf_data` is NULL.
    # So, try to fetch growth profile data from DataBank.
    #
    if (is.null(gf_data)) {

      msg("gf_data <- DB_growth_profiles()")
      gf_data <-
        DB_growth_profiles(
          base_year = base_year,
          years = years,
          na.rm = na.rm,
          verbose = verbose)

    }

    # Never bother with any extraneous GFs
    gf_data <-
      semi_join(
        filter(gf_data, year %in% years),
        tput_data,
        by = "cat_id")

    if ("cnty_abbr" %in% names(tput_data))  {

      # In the `tput_data`, when cnty_abbr is present, there should be no cases where it is NA.
      if (any(is.na(tput_data[["cnty_abbr"]]))) {
        err_msg <- "you have NAs in `cnty_abbr` (in the data you are trying to project)."
        stop(err_msg)
      }

      # In the `gf_data`, however, some (or all) values of `cnty_abbr` might be NA.
      # The remainder should be specific counties ("ALA", "CC", ... "SON").
      #
      # If the column isn't even present in `gf_data`, we'll have to treat
      # it as though it were all NA. We're here because `cnty_abbr` is present in
      # `tput_data`, so we'll need something to join against.
      #
      gf_data <-
        gf_data %>%
        expand_counties(
          verbose = verbose)

    } else {

      if ("cnty_abbr" %in% names(gf_data)) {
        err_msg <- "`cnty_abbr` is in your growth data, but not in the data you are trying to project"
        stop(err_msg)
      }

    }

    projected_tput_data <-
      tput_data %>%
      project_annual_throughputs_by(
        cat_id,
        cnty_abbr,
        using = gf_data,
        verbose = verbose)

  }


  if (is.null(cf_data)) {

    if (is.null(base_year)) {

      cf_data <-
        tibble(
          year = character(0),
          cat_id = integer(0),
          pol_abbr = character(0),
          pol_id = integer(0),
          cf_qty = numeric(0))

    } else {

      msg("legacy_format_cf_data <- DB_control_factors()")
      legacy_format_cf_data <-
        DB_control_factors(
          base_year = base_year,
          na.rm = na.rm,
          verbose = verbose) %>%
        elide_years(
          verbose = verbose) %>%
        semi_join(
          tput_data,
          by = "cat_id")

      msg("cf_data <- annualize_DB_control_factors(legacy_format_cf_data)")
      cf_data <-
        legacy_format_cf_data %>%
        annualize_DB_control_factors(
          years = years,
          verbose = verbose)

    }

  }

  # don't bother with any extraneous CFs
  cf_data <-
    semi_join(
      filter(cf_data, year %in% years),
      tput_data,
      by = "cat_id")

  if (is.null(ef_data)) {

    msg("legacy_format_ef_data <- DB_area_source_emission_factors()")
    legacy_format_ef_data <-
      base_year %>%
      DB_area_source_emission_factors(
        na.rm = na.rm,
        verbose = verbose) %>%
      semi_join(
        tput_data,
        by = "cat_id")

    msg("ef_data <- annualize_DB_emission_factors(legacy_format_ef_data)")
    ef_data <-
      legacy_format_ef_data %>%
      annualize_DB_emission_factors(
        years = years,
        verbose = verbose)

  }

  msg("projecting throughputs from ", min(years), " to ", max(years))

  # join projected throughput data with emission factors
  joined_data <-
    projected_tput_data %>%
    inner_join(
      ef_data,
      by = c("year", "cat_id"))

  # don't bother with any extraneous EFs
  ef_data <-
    ef_data %>%
    filter(
      year %in% years) %>%
    semi_join(
      tput_data,
      by = "cat_id")

  if (nrow(cf_data) > 0) {

    # join again, this time with control factors
    pol_vars <-
      intersect(
        tidyselect::vars_select(names(joined_data), starts_with("pol_")),
        tidyselect::vars_select(names(cf_data), starts_with("pol_")))

    msg("joining with control factors")

    joined_data <-
      joined_data %>%
      left_join(
        cf_data,
        by = c("year", "cat_id", pol_vars))

  } else {

    msg("no control factors; setting `cf_qty` to `NA`")

    joined_data <-
      joined_data %>%
      mutate(
        cf_qty = NA_real_)

  }

  msg("calculating: ems_qty = tput_qty x ef_qty x cf_qty")
  multiplied_data <-
    joined_data %>%
    replace_na(
      list(cf_qty = 1.0)) %>%  # default `cf` is 100% (uncontrolled)
    mutate(
      ems_qty = tput_qty * ef_qty * cf_qty,
      ems_unit = "lbs/yr") %>% # because in DataBank, `ef_qty` is "lbs/tput"
    convert_emission_units(
      from = "lbs/yr",
      to = "ton/yr")

  msg("tidying")

  tidied_data <-
    multiplied_data %>%
    # mutate(
    #   pol_abbr = decode(pol_id, DB_PROJECTED_POLLUTANT_CODES)) %>%
    dplyr::select(
      one_of(names(tput_data)),
      starts_with("pol"),
      one_of(names(ef_data)),
      one_of(names(cf_data)),
      ems_qty,
      ems_unit) %>%
    mutate_at(
      vars(ends_with("_id")),
      ~ as.integer(.))

  if (isTRUE(na.rm)) {

    msg("dropping rows where ems_qty == 0")

    tidied_data <-
      tidied_data %>%
      filter(
        ems_qty > 0)

  }

  return(tidied_data)

}

#' @name BY_area_source_projections
#'
#' @export
BY_area_source_projections <-
  memoise::memoise(
    BY_area_source_projections_,
    cache = memoise::cache_memory())

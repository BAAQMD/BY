context("area_source_projections (BY2011)")

QA_base_year <- BY(2011)

#'-----------------------------------------------------------------------------

test_that("manual entry of CY2011 throughput for #283", {

  tput_data <-
    tibble(
      year = CY(2011), # was: CY(2011:2015)
      cat_id = 283L,
      cnty_abbr = c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON"),
      tput_qty = c(13625, 10436, 3283, 1250, 8858, 7775, 15303, 2554, 3771),
      tput_unit = "millions of ft^3")

  projected_data <-
    area_source_projections_(  # be sure to test the *uncached* variant (w/trailing underscore)
      QA_base_year,
      years = CY(2011, 2030),
      tput_data = tput_data,
      verbose = TRUE) %>%
    filter(
      pol_abbr == "NOx")

  #
  # This shows +9% growth (CY2011 -> 2030).
  #
  projected_data %>%
    filter(
      year %in% CY(2011, 2030),
      pol_abbr == "NOx")

  #
  # But published data shows +24% growth (CY2011 -> 2030).
  #
  BY2011::BY2011_annual_emission_data %>%
    filter(
      cat_id == 283L,
      year %in% CY(2011, 2030),
      pol_abbr == "NOx")

  #
  # DataBank says category #283 is mapped to growth profile #555.
  #
  BY(2011) %>%
    DB_growth_profile_crosswalk() %>%
    filter(
      cat_id == 283)

  #
  # And DB growth profile #555 shows 1.12 / 1.03 ≈ +9% growth.
  #
  # Why doesn't this match published data?
  #
  BY(2011) %>%
    DB_raw_growth_profiles() %>%
    filter(
      gpf_id == 555) %>%
    unnest(
      gpf_raw_data) %>%
    filter(
      year %in% CY(2011, 2030))

})

#'-----------------------------------------------------------------------------

#
# As above, one can supply `area_source_projections()` with explicit
# `tput_data`, `ef_data`, `cf_data`, and/or `gpf_data`, but below
# we are testing on the default behavior, which is to rely on
# `DB_area_source_*(base_year, ...)`.
#
QA_area_source_projection_data <-
  QA_base_year %>%
  area_source_projections(
    verbose = TRUE)

#'-----------------------------------------------------------------------------

test_that("county-level projections for category #1010, CY2030", {

  QA_year <- CY(2030)

  # throughputs only
  QA_cat_1010_projection_data <-
    QA_area_source_projection_data %>%
    filter(
      cat_id == 1010L) %>%
    filter(
      elide_year(year) == QA_year) %>%
    annual_throughputs_by(
      cnty_abbr,
      tput_unit) %>%
    mutate_at(
      vars(tput_qty),
      ~ round(., digits = 0))

  #
  # Expect an empty data frame.
  #
  # See email from Michael Nguyen dated Wednesday, August 28, 2019 7:38 AM:
  #
  # "Category#1010 is classified as “CARB” type (screenshot below). The CARB’s
  #  emissions for this Category was incorporated into BY2011 Emission Inventory.
  #  Therefore, throughput for this category was not entered in the q10 data
  #  field of FID1325 (BY2011). Throughput information was not available from
  #  CARB’s CEPAM [tool]."
  #
  QA_cat_1010_projection_data %>%
    nrow() %>%
    expect_equal(0)

})

#'-----------------------------------------------------------------------------

test_that("projected county-level throughputs for category #27, year 2030", {

  QA_cat_id <- 27L
  QA_year <- 2030L

  QA_area_source_projection_data %>%
    filter(
      cat_id == QA_cat_id) %>%
    filter(
      elide_year(year) == QA_year) %>%
    annual_throughputs_by(
      cnty_abbr,
      tput_unit) %>%
    mutate_at(
      vars(tput_qty),
      ~ round(., digits = 0)) %>%
    expect_equal(
      tibble(
        year = "CY2030",
        cnty_abbr = c("ALA", "CC", "NAP", "SF", "SM"),
        tput_qty = c(819, 14252, 164, 819, 328),
        tput_unit = "No. Components"))

})

#'-----------------------------------------------------------------------------

test_that("projected county-level TOG emissions for category #66, CY2003:2011", {

  # throughputs, emission factors, control factors, and emissions
  QA_cat_66_projection_data <-
    QA_area_source_projection_data %>%
    filter(
      cat_id == 66L) %>%
    filter(
      elide_year(year) %in% c(2003:2011))

  #'
  #' Transcribed from email from Michael Nguyen
  #' (Thursday, September 5, 2019 at 5:04 PM)
  #'
  QA_cat_66_projection_data %>%
    convert_emission_units(
      to = "tons/day") %>%
    annual_emissions_by(
      cat_id,
      pol_abbr) %>%
    pull(
      ems_qty) %>%
    expect_equal(
      c(1.282, 0.98, 0.908, 0.901, 0.892, 0.856, 0.843, 0.846, 0.831),
      tol = 0.001)

  expected <-
    tibble(
      year = "CY2011",
      cat_id = 66L,
      cnty_abbr = c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON"),
      tput_qty = c(593430, 352614, 122593, 50887, 146751, 283479, 646888, 198153, 175279),
      tput_unit = "1000 Gallons Gasolin",
      pol_abbr = "TOG",
      pol_id = 990L,
      ef_qty = 1.34,
      cf_qty = 0.176,
      ems_qty = c(70.1, 41.6, 14.5, 6.01, 17.3, 33.5, 76.4, 23.4, 20.7),
      ems_unit = "tons/yr")

  QA_cat_66_projection_data %>%
    filter(
      elide_year(year) == 2011L) %>%
    mutate_at(
      vars(tput_qty),
      ~ round(., digits = 0)) %>%
    mutate_at(
      vars(ef_qty, cf_qty, ems_qty),
      ~ signif(., digits = 3)) %>%
    expect_equal(
      expected)

})


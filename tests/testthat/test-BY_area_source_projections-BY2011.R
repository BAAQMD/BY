context("BY_area_source_projections (BY2011)")

#'-----------------------------------------------------------------------------

#
# You can supply `BY_area_source_projections` with explicit
# `tput_data`, `ef_data`, `cf_data`, and/or `gpf_data`,
# but here we are relying on the default behavior, which
# is to rely on `DB_area_source_*()`.
#
BY2011_area_source_projection_data <-
  BY_area_source_projections(
    base_year = BY(2011),
    verbose = TRUE) %>%
  as_tibble()

test_that("county-level projections for category #1010, year 2030", {

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
  BY2011_area_source_projection_data %>%
    filter_categories(
      1010) %>%
    filter_years(
      CY(2030)) %>%
    annual_throughputs_by(
      cnty_abbr,
      tput_unit) %>%
    nrow() %>%
    expect_equal(0)

})

#'-----------------------------------------------------------------------------

test_that("projected county-level throughputs for category #27, year 2030", {

  expected <-
    tibble(
      year = CY(2030),
      cnty_abbr = c("ALA", "CC", "NAP", "SF", "SM"),
      tput_qty = c(819, 14252, 164, 819, 328),
      tput_unit = "No. Components")

  BY2011_area_source_projection_data %>%
    filter_categories(
      27) %>%
    filter_years(
      CY(2030)) %>%
    sum_annual_throughputs_by(
      cnty_abbr,
      tput_unit,
      digits = 0) %>%
    as_tibble() %>% # drop any extra classes, like "inventory"
    expect_equal(
      expected)

})

#'-----------------------------------------------------------------------------

test_that("category #66 county-level TOG (CY2003:2011)", {

  #'
  #' Transcribed from email from Michael Nguyen
  #' (Thursday, September 5, 2019 at 5:04 PM)
  #'
  expected <-
    tibble(
      ems_qty = c(1.282, 0.98, 0.908, 0.901, 0.892, 0.856, 0.843, 0.846, 0.831),
      ems_unit = "ton/day")

  BY2011_area_source_projection_data %>%
    filter_categories(
      66) %>%
    filter_years(
      CY(2003:2011)) %>%
    convert_emission_units(
      to = "ton/day") %>%
    sum_annual_emissions_by(
      cat_id,
      pol_abbr,
      digits = 3) %>%
    as_tibble() %>% # drop any extra classes, like "inventory"
    select(
      ems_qty,
      ems_unit) %>%
    expect_equal(
      expected,
      tol = 0.001)

})

test_that("category #66 regional tput, ef, cf, and ems (CY2011)", {

  expected <-
    tibble(
      year = CY(2011),
      cat_id = 66L,
      cnty_abbr = c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON"),
      tput_qty = c(593430, 352614, 122593, 50887, 146751, 283479, 646888, 198153, 175279),
      tput_unit = "1000 Gallons Gasolin",
      pol_id = 990L,
      pol_abbr = "TOG",
      ef_qty = 1.34,
      ef_unit = "lb/tput",
      cf_qty = 0.176,
      ems_qty = c(70.1, 41.6, 14.5, 6.01, 17.3, 33.5, 76.4, 23.4, 20.7),
      ems_unit = "ton/yr")

  BY2011_area_source_projection_data %>%
    filter(
      cat_id == 66) %>%
    filter_years(
      CY(2011)) %>%
    mutate_at(
      vars(tput_qty),
      ~ round(., digits = 0)) %>%
    mutate_at(
      vars(ef_qty, cf_qty, ems_qty),
      ~ signif(., digits = 3)) %>%
    expect_equal(
      expected)

})


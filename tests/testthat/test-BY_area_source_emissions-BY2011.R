context("BY_area_source_emissions (BY2011)")

BY2011_area_source_emission_data <-
  BY_area_source_emissions(
    BY(2011),
    verbose = TRUE)

#'-----------------------------------------------------------------------------

test_that("names", {

  BY2011_area_source_emission_data %>%
    names() %>%
    expect_setequal(
      c("year",
        "cat_id",
        "cnty_abbr",
        "tput_qty", "tput_unit",
        "pol_abbr", "pol_id",
        "ef_qty", "ef_unit",
        "cf_qty",
        "ems_qty", "ems_unit"))

})

test_that("BY2011 years", {

  BY2011_area_source_emission_data %>%
    pull(year) %>%
    elide_year() %>%
    expect_setequal(2011L)

})

test_that("BY2011 counties", {

  BY2011_area_source_emission_data %>%
    pull(cnty_abbr) %>%
    expect_setequal(names(DB_COUNTY_CODES))

})

#'-----------------------------------------------------------------------------

test_that("PM emissions for category #1908", {

  expected <-
    tibble(
      year = CY(2011),
      cat_id = 1908L,
      cnty_abbr = names(DB_COUNTY_CODES),
      pol_id = 1990L,
      pol_abbr = "PM",
      ems_qty = c(104, 27.3, 5.99, 6.35, 11.2, 6.13, 65.0, 19.6, 29.1),
      ems_unit = "ton/yr")

  BY2011_area_source_emission_data %>%
    filter(
      cat_id == 1908L) %>%
    dplyr::select(
      year,
      cat_id,
      cnty_abbr,
      starts_with("pol_"),
      starts_with("ems_")) %>%
    mutate_at(
      vars(ems_qty),
      ~ signif(., digits = 3)) %>%
    expect_equal(
      expected)

})


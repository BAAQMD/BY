context("expand_counties")

COUNTY_ABBRS <-
  c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON")

test_that("if cnty_abbr is `NA`, it's expanded to all counties", {

  test_data <-
    tibble(
      cat_id = 283,
      cnty_abbr = NA_character_,
      gf_qty = 1.01)

  expected <-
    tibble(
      cat_id = 283,
      cnty_abbr = COUNTY_ABBRS,
      gf_qty = 1.01)

  test_data %>%
    expand_counties() %>%
    expect_equal(
      expected)

})

test_that("cnty_abbr is not present", {

  test_data <-
    tibble(
      cat_id = 283,
      gf_qty = 1.01)

  expected <-
    tibble(
      cat_id = 283,
      cnty_abbr = COUNTY_ABBRS,
      gf_qty = 1.01)

  test_data %>%
    expand_counties() %>%
    expect_equal(
      expected)

})

test_that("cnty_abbr is a subset of COUNTY_ABBRS", {

  test_data <-
    tibble(
      cat_id = 283,
      cnty_abbr = c("ALA", "CC", "SON"),
      gf_qty = 1.01)

  expected <-
    tibble(
      cat_id = 283,
      cnty_abbr = c("ALA", "CC", "SON"),
      gf_qty = 1.01)

  test_data %>%
    expand_counties() %>%
    expect_equal(
      expected)

})

test_that("cnty_abbr is a mix of TOT and county codes", {

  test_data <-
    tibble(
      cat_id = c(283, 283, 99),
      cnty_abbr = c("ALA", "CC", "TOT"),
      gf_qty = 1.01)

  expected <-
    tibble(
      cat_id = c(c(283, 283), rep(99, length(COUNTY_ABBRS))),
      cnty_abbr = c("ALA", "CC", COUNTY_ABBRS),
      gf_qty = 1.01)

  test_data %>%
    expand_counties() %>%
    expect_equal(
      expected)

})

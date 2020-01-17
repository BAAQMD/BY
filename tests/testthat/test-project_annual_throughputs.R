context("project_annual_throughputs_by")

# residential natural gas (NG) combustion used for space heating
gf_data <-
  tibble(
    cat_id = 283L,
    year = CY(2015:2030)) %>%
  mutate(
    gf_qty = (1.03) ^ (0:(n() - 1)))

test_that("regional, single category", {

  tput_data <-
    tibble(
      year = CY(2015),
      cat_id = 283L,
      tput_qty = 52400) # natural gas, millions of ft^3

  projected_data <-
    tput_data %>%
    project_annual_throughputs_by(
      cat_id,
      using = gf_data,
      verbose = TRUE)

  expected <-
    pull(tput_data, tput_qty) *
    pull(gf_data, gf_qty)

  projected_data %>%
    pull(
      tput_qty) %>%
    expect_equal(
      expected)

})

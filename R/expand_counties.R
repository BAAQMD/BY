#' Expand all cases where `cnty_abbr` is not in `cnty_codes`,
#' replacing with the full set of county codes.
#'
expand_counties <- function (
  input_data,
  cnty_var = "cnty_abbr",
  cnty_codes = NULL,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[expand_counties] ", ...)

  if (cnty_var %not_in% names(input_data))  {
    input_data[[cnty_var]] <- NA_character_
  }

  msg("cnty_var is: ", cnty_var)

  if (is.null(cnty_codes)) {
    cnty_codes <- c("ALA", "CC", "MAR", "NAP", "SF", "SM", "SNC", "SOL", "SON")
    msg("cnty_codes defaulting to: ", str_csv(cnty_codes))
  } else {
    msg("cnty_codes is: ", str_csv(cnty_codes))
  }

  valid_codes <-
    c("TOT", cnty_codes, NA_character_)

  stopifnot(
    all_true(input_data[[cnty_var]] %in% valid_codes))

  cleaned_data <-
    input_data %>%
    mutate_at(
      vars(!!cnty_var),
      ~ if_else(. %in% cnty_codes, ., NA_character_))

  expanded_data <-
    cleaned_data %>%
    mutate_at(
      vars(!!cnty_var),
      ~ map(., if_na_then, replacement = cnty_codes)) %>%
    tidyr::unchop(
      !!cnty_var)

  return(expanded_data)

}

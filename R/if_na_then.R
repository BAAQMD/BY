if_na_then <- function (x, replacement) {
  if (is.na(x)) {
    return(replacement)
  } else {
    return(x)
  }
}

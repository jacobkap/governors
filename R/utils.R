# Removes non-number parts of columns
remove_paren <- function(x) {
  x <- gsub("\\(.*", "", x)
  x <- gsub("^[^[:alnum:]]+$", NA, x)
  return(x)
}


# Makes the regular_term_in_years years from 2-digits to 4-digits
two_to_four_years <- function(years) {
  years <- gsub("-9", "-199", years)
  years <- gsub("-2", "-202", years)
  years <- gsub("-0", "-200", years)
  years <- gsub("-1", "-201", years)

  return(years)
}

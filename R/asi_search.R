#'Pull Tidy Data for MHC from LERS using search string
#'@param search_string A string with a measure 'by' some row.
#'@param postalcodes Optional: Filters by Postal Code. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param censusdivisions Optional: Filters by Census Division. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param cipcodes Optional: Filters by Cip Code of any length, can used mixed vector with #'any length cip codes together. Can exclude by including "exclude" in the vector, then the items in that vector #'will be excluded rather than included.
#'@param ages Optional: Filters by age group.
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@param print Optional: Prints the MDX string used to query the database.
#'
#'@examples
#' df <- asi_search("FLE by Gender", sa.mh = T, ages = 2)
#'@export asi_search

asi_search <- function(search_string, ...) {

  df = tibble(Measure = search_string) %>%
    separate(
      Measure,
      into = c("Measure", "Rows"),
      sep = " by ",
      fill = "right"
    )

  m = df$Measure
  r = df$Rows

  if (is.na(r)) {
    r <- NULL
  } else if (str_detect(r, " and ")){
    r <-  r %>%
      str_split(" and ", simplify = T) %>%
      as.character()
  }


  get_applications(m, r, ...)
}

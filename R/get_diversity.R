#'Calculate a Simpson Diversity Index for LERS Data
#'@param rows Select rows to cut data by
#'@param species Element of diversity
#'@param institutions Optional: Filters by Institution
#'@param type Optional: Type of Simpson Diversity Index, Defaults to "Dominance"
#'@export get_diversity
#'
get_diversity <- function(species, rows, institutions, type) {
  if (missing(type)) {
    type = "dominance"
  }
  #if(missing(inst)){inst = c()}
  if (missing(rows)) {
    rows = c()
  }

  sdi2 <- function (col, data, type)
  {
    data$number2 <- data[col] * (data[col] - 1)
    Numer <- sum(data$number2)
    Denom <- sum(data[col]) * (sum(data[col]) - 1)
    if (type == "dominance") {
      DI <- 1 - (Numer / Denom)
    }
    else if (type == "reciprocal") {
      DI <- 1 / (Numer / Denom)
    }
    else if (type == "SI") {
      DI <- Numer / Denom
    }
    return(DI)
  }

  df <-
    get_enrolment("Unique Student Static", c(species, rows), institutions = institutions) %>%
    group_by_(.dots = c(lapply(c(
      "Academic Year", rows
    ), as.symbol))) %>%
    do(summarise(., Diversity = sdi2(
      col = "Unique Student Static",
      data = ., type = type
    )))


  return(df)
}

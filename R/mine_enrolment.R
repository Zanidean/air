#'Mine LERS data for permutations of rows and detect significant changes.
#'@param measures Select a measurement.
#'@param rows Select elements in LERS to permutate combinations of.
#'@param institutions Optional: Supplys permutations for institutions.
#'@param postalcodes Optional: Filters by Postal Code. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param censusdivisions Optional: Filters by Census Division. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param cipcodes Optional: Filters by Cip Code of any length, can used mixed vector with any length cip codes together.
#'@param ages Optional: Filters by age group.
#'@param detect Use xmrr to detect significant changes.
#'@examples
#' df <- mine_enrolment("FLE",
#' c("Program Type", "Credential Type", "Gender"),
#' institutions = c("MR", "MH", "UA"),
#' ages = 2, sa.mh = T)
#'@export mine_enrolment

mine_enrolment <- function(measure,
                           rows,
                           institutions = "MH",
                           postalcodes = c(),
                           censusdivisions = c(),
                           cipcodes = c(),
                           ages = c(),
                           sa.mh = F,
                           detect_change = F,
                           min_year, max_year) {

  require(purrr)

  if(missing(measure)){
    warning("You need to supply a measure.")
    stop()
  }

  if(missing(rows)){
    warning("You need to supply some rows to permutate.")
    stop()
  }
  if (missing(sa.mh)) {
    sa.mh <- F
  }
  if (missing(postalcodes)) {
    postalcodes <- NA
  }
  if (missing(censusdivisions)) {
    censusdivisions <- NA
  }
  if (missing(ages)) {
    ages <- NA
  }
  if (missing(cipcodes)) {
    cipcodes <- NA
  }
  if (missing(min_year)) {
    min_year <- NA
  }
  if (missing(max_year)) {
    max_year <- NA
  }


  # this is where you calculate the permutations of each input
  ro <- rows
  ins <- institutions

  d <- expand.grid(ins = ins, ro = ro) %>%
    mutate(ro = as.character(ro),
           ins = as.character(ins))

  # this is where you map each input to the function, and clean up data

  if(detect_change == T){
    require(xmrr)
    # This is a function to determine if and by how much an XMR changes
    xmr_change <- function(dataframe){

      # Defining Range for Change
      ty_min <- dataframe$`Academic Year` %>% unique %>% min(na.rm = T)


      if(min_year %in% dataframe$`Academic Year`){
        ty_min <- min_year
      }

      ty_max <- dataframe$`Academic Year` %>% unique %>% max(na.rm = T)
      if(max_year %in% dataframe$`Academic Year`){
        ty_min <- max_year
      }

      # this determines the 'central line' delta
      c_delta <- dataframe$`Central Line`[dataframe$`Academic Year` == ty_max] -
        dataframe$`Central Line`[dataframe$`Academic Year` == ty_min]

      # this determines the overall enrolment delta
      e_delta <- dataframe[[measure]][dataframe$`Academic Year` == ty_max] -
        dataframe[[measure]][dataframe$`Academic Year` == ty_min]

      # this adds columns to data to determine change.
      dataframe %>%
        mutate(Central_Delta = c_delta,
               Enrol_Delta = e_delta,
               Delta_Range = paste0(ty_min, " to ", ty_max),
               Change = ifelse(sign(Central_Delta) == -1, "Negative", "Positive"),
               Change = ifelse(Central_Delta == 0, "No Change", Change))
    }

    dat <- map2(d$ro, d$ins, function(ro, ins){
      get_enrolment(measure,
                    ro,
                    ins,
                    postalcodes = postalcodes,
                    censusdivisions = censusdivisions,
                    cipcodes = cipcodes,
                    ages = ages,
                    sa.mh = sa.mh) %>%
        mutate(Factor = ro,
               Institution = ins) %>%
        rename(Variable = 2) %>%
        group_by(Variable) %>%
        do(xmr(., measure)) %>%
        ungroup
    }) %>% bind_rows %>%
      group_by(Institution, Factor, Variable) %>%
      nest %>%
      mutate(data = map(data, xmr_change)) %>%
      unnest

  } else {
    dat <- map2(d$ro, d$ins, function(ro, ins){
      get_enrolment(measure,
                    ro,
                    ins,
                    postalcodes = postalcodes,
                    censusdivisions = censusdivisions,
                    cipcodes = cipcodes,
                    ages = ages,
                    sa.mh = sa.mh) %>%
        mutate(Factor = ro,
               Institution = ins) %>%
        rename(Variable = 2)
    }) %>% bind_rows %>%
      group_by(Institution, Factor, Variable) %>%
      nest %>%
      unnest
  }
  return(dat)
}




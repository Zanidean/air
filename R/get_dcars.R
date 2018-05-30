#'Pull Tidy Data from both ASI and LERS
#'@param app_measures Select possible measurements from ASI
#'@param enrol_measures Select possible measurements from LERS
#'@param institutions Filters by Institution
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username"
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password"
#'
#'@examples
#'df <- get_dcars(app_measures = c("Unique Applicant Static"),
#'            enrol_measures = c("FLE"),
#'            rows = c("Provider", "Gender"),
#'            institutions = c("MH", "MU", "UA"))
#'@export get_dcars

get_dcars <- function(app_measures, enrol_measures, rows, institutions){
  app <- get_applications(app_measures, rows, institutions)
  enrol <- get_enrolment(enrol_measures, rows, institutions)
  df <- dplyr::full_join(app, enrol, by = c("Academic Year", rows))
  return(df)
}



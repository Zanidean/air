#'Pull data from LERS
#'@param rows Select rows to cut data by
#'@param rate Either Conversion or Acceptance rate
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@export get_rate
#'
get_rate <- function(rows, rate){

  qualified <- get_applications("Unique Applicant Static", c(rows, "Qualified")) %>%
    filter(Qualified == "Qualified") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select(`Academic Year`, rows, `Unique Applicant Static`) %>%
    rename(Qualified = `Unique Applicant Static`)

  offered <- get_applications("Unique Applicant Static", c(rows, "Qualified", "Offered")) %>%
    filter(Qualified == "Qualified", Offered == "Offered") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select(`Academic Year`, rows, `Unique Applicant Static`) %>%
    rename(Offered = `Unique Applicant Static`)

  attending <- get_applications("Unique Applicant Static", c(rows, "Qualified", "Offered", "Attending")) %>%
    filter(Qualified == "Qualified", Offered == "Offered", Attending == "Attending") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select(`Academic Year`, rows, `Unique Applicant Static`) %>%
    rename(Attending = `Unique Applicant Static`)

  df <- full_join(qualified, offered, by = c("Academic Year", rows)) %>%
    full_join(attending, by = c("Academic Year", rows)) %>%
    mutate(`Conversion Rate` = Attending/Offered,
           `Acceptance Rate` = Offered/Qualified) %>%
    select(`Academic Year`, rows, rate)

  return(df)
}

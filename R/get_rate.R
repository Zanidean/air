#'Pull data from ASI
#'@param rows Select rows to cut data by. Defaults to "Provider"
#'@param rate Either Conversion or Acceptance rate
#'@param institutions Optional: Filters by Institution
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@export get_rate
#'
get_rate <- function(rate, rows, institutions, sa.mh = F, postalcodes = c()){


  #if(missing(institutions)){institutions <- c()}
  if(missing(rows)){rows <- NULL}
  if(missing(sa.mh)){sa.mh <- F}
  if(missing(postalcodes)){postalcodes <- NA}


  qualified <- get_applications("Unique Applicant Static", c(rows, "Qualified"),
                                institutions = institutions,
                                sa.mh = sa.mh, postalcodes = postalcodes) %>%
    filter(Qualified == "Qualified") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select_(.dots = c(lapply(c("Academic Year", rows, "Unique Applicant Static"), as.symbol))) %>%
    rename(Qualified = `Unique Applicant Static`)

  offered <- get_applications("Unique Applicant Static", c(rows, "Qualified", "Offered"),
                              institutions = institutions,
                              sa.mh = sa.mh, postalcodes = postalcodes) %>%
    filter(Qualified == "Qualified", Offered == "Offered") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select_(.dots = c(lapply(c("Academic Year", rows, "Unique Applicant Static"), as.symbol))) %>%
    rename(Offered = `Unique Applicant Static`)

  attending <- get_applications("Unique Applicant Static", c(rows, "Qualified", "Offered", "Attending"),
                                institutions = institutions,
                                sa.mh = sa.mh, postalcodes = postalcodes) %>%
    filter(Qualified == "Qualified", Offered == "Offered", Attending == "Attending") %>%
    group_by_(.dots = c(lapply(c("Academic Year", rows), as.symbol))) %>%
    summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
    select_(.dots = c(lapply(c("Academic Year", rows, "Unique Applicant Static"), as.symbol))) %>%
    rename(Attending = `Unique Applicant Static`)

  df <- full_join(qualified, offered, by = c("Academic Year", rows)) %>%
    full_join(attending, by = c("Academic Year", rows)) %>%
    mutate(`Conversion Rate` = Attending/Offered,
           `Acceptance Rate` = Offered/Qualified) %>%
    select_(.dots = c(lapply(c("Academic Year", rows, rate), as.symbol)))

  return(df)
}

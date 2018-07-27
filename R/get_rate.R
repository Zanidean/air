#'Calculate Rates from DCARS data
#'@param rows Select rows to cut data by. Defaults to "Provider"
#'@param rate Either Conversion, Acceptance, or Capture Rates
#'@param institutions Optional: Filters by Institution
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@export get_rate
#'
get_rate <- function(rate,
                     rows,
                     institutions,
                     sa.mh = F,
                     postalcodes = c(),
                     censusdivisions = c()) {
  #if(missing(institutions)){institutions <- c()}
  if (missing(rows)) {
    rows <- NULL
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

  if (rate != "Capture Rate") {
    qualified <-
      get_applications(
        "Unique Applicant Static",
        c(rows, "Qualified"),
        institutions = institutions,
        sa.mh = sa.mh,
        postalcodes = postalcodes,
        censusdivisions = censusdivisions
      ) %>%
      filter(Qualified == "Qualified") %>%
      group_by_(.dots = c(lapply(c(
        "Academic Year", rows
      ), as.symbol))) %>%
      summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
      select_(.dots = c(lapply(
        c("Academic Year", rows, "Unique Applicant Static"),
        as.symbol
      ))) %>%
      rename(Qualified = `Unique Applicant Static`)

    offered <-
      get_applications(
        "Unique Applicant Static",
        c(rows, "Qualified", "Offered"),
        institutions = institutions,
        sa.mh = sa.mh,
        postalcodes = postalcodes,
        censusdivisions = censusdivisions
      ) %>%
      filter(Qualified == "Qualified", Offered == "Offered") %>%
      group_by_(.dots = c(lapply(c(
        "Academic Year", rows
      ), as.symbol))) %>%
      summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
      select_(.dots = c(lapply(
        c("Academic Year", rows, "Unique Applicant Static"),
        as.symbol
      ))) %>%
      rename(Offered = `Unique Applicant Static`)

    attending <-
      get_applications(
        "Unique Applicant Static",
        c(rows, "Qualified", "Offered", "Attending"),
        institutions = institutions,
        sa.mh = sa.mh,
        postalcodes = postalcodes,
        censusdivisions = censusdivisions
      ) %>%
      filter(Qualified == "Qualified",
             Offered == "Offered",
             Attending == "Attending") %>%
      group_by_(.dots = c(lapply(c(
        "Academic Year", rows
      ), as.symbol))) %>%
      summarise(`Unique Applicant Static` = sum(`Unique Applicant Static`, na.rm = T)) %>%
      select_(.dots = c(lapply(
        c("Academic Year", rows, "Unique Applicant Static"),
        as.symbol
      ))) %>%
      rename(Attending = `Unique Applicant Static`)

    df <-
      full_join(qualified, offered, by = c("Academic Year", rows)) %>%
      full_join(attending, by = c("Academic Year", rows)) %>%
      mutate(`Conversion Rate` = Attending / Offered,
             `Acceptance Rate` = Offered / Qualified) %>%
      select_(.dots = c(lapply(
        c("Academic Year", rows, rate), as.symbol
      )))

    return(df)
  } else if (rate == "Capture Rate") {
    # total <- get_enrolment("FLE",
    #                        rows = c("Provider Service Area", rows)) %>%
    #   rename(Total = FLE)
    # inst <- get_enrolment("FLE",
    #                       rows = c("Provider Service Area", "Provider", rows),
    #                       i = institutions)
    #
    # df <- left_join(inst, total) %>%
    #   mutate(`Capture Rate` = FLE/Total) %>%
    #   select(-FLE, -Total)
    warning("Capture Rate doesn't work!")

    return(df)
  }
}

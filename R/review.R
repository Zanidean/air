review <- function(rows, institutions = NA, xmr = F, recalc = T){
  vars <- c("Provider", rows)
  inst <- institutions

  r <- enrolment(c("FLE", "Unique Student Static"),
                 c(vars), inst) %>%
    gather(Measure, Value, -(1:(length(vars)+1)))

  app <- applications(c("Application Record Count", "Unique Applicant Static"),
                      c(vars), inst) %>%
    gather(Measure, Value, -(1:(length(vars)+1)))

  df <- rbind(r, app)

  if(xmr == T){
    group_by_(.dots = c(lapply(c(vars, "Measure"), as.symbol))) %>%
    do(xmR(., "Value", recalc = recalc)) %>%
    select(-Order)
  }
  return(df)
}

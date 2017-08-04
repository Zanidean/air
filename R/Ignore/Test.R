library(air)

df <- dcars(app_measures = c("Unique Applicant Static",
                             "Application Record Count"),
            enrol_measures = c("FLE",
                               "Unique Student Static"),
            rows = c("Provider", "Gender", "Credential Type",
                     "Program Band", "Aboriginal Indicator"),
            institutions = c("MH", "MU", "UA"))


app <- applications("Unique Applicant Static", c("Credential Type", "Program Name",
                                                 "Qualified", "Offered", "Attending"),
                    "MH")

#'Elements of DCARS that air packages can use.
#'@param cube Return a list of possible measures and rows to query using AIR in that cube
#'
#'@examples
#'air_vars("LERS")
#'
#'@export air_vars

air_vars <- function(cube){
  errormessage = "Please supply a valid cube. Either \"ASI\" or \"LERS\" is acceptable."
  if(missing(cube)){message(errormessage)}
##################################################
  if(cube == "LERS"){
    message("LERS")
    measures <- c("Unique Student Static","Unique Student Current","FLE",
              "Calculated Full-Time for Year","Calculated Part-Time for Year")
    rows <- c(#tombstone
              "Gender","Legal Status","Age Group",
              "Age","Source Country","Aboriginal Indicator",
              "Country Of Citizenship","Language",
              "Grade Completed Year","Current Status",
              #registration
              "Year Of Study","Session","Level Of Study",
              "Current Status","Registration Status",
              "Registration Type","Level of Study",
              "Calculated Learned Registration Status",
              "Attainment",
              #provider
              "Provider","Provider Location",
              "Provider Sector", "Provider Service Area",
              #service area
              "Country", "Province",
              "Service Area", "Postal Code",
              "Census Division",
              #program
              "Credential Type", "Program Type",
              "Program Band", "Program Name",
              "Program Name Code", "Program Specialization",
              "Program Specialization Code",
              "Program Length", "CIP Level 2",
              "CIP Level 4", "CIP Level 6")

    message("Measures")
    print(measures)
    message("Rows")
    print(rows)

##################################################
  } else if (cube == "ASI"){
    message("ASI")
    measures <- c("Application Record Count",
              "Unique Applicant Current",
              "Unique Applicant Static")

    rows <- c(#tombstone
              "Gender", "Age Group", "Age",
              "Aboriginal Indicator",
              #registration
              "Qualified", "Offered", "Attending",
              "Level Of Study", "Current Status",
              "Registration Status",
              #provider
              "Provider","Provider Sector",
              #service area
              "Country","Province",
              "Service Area","Postal Code",
              #program
              "Program Name", "Program Name Code",
              "Credential Type", "Program Type",
              "Program Band","Program Specialization",
              "Program Specialization Code",
              "CIP Level 2", "CIP Level 4","CIP Level 6")


    message("Measures")
    print(measures)
    message("Rows")
    print(rows)

##################################################
  } else {message(errormessage)}
}

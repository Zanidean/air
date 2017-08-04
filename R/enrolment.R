#'Pull data from LERS
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
#'@param institutions Optional: Filters by Institution
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@param print Optional: Prints the MDX string used to query the database.
#'
#'@examples
#' df <- enrolment(measures = c("Unique Student Static", "FLE"),
#'                 rows = c("Provider", "Gender"),
#'                 institutions = c("MH", "MU", "UA"))
#'@export enrolment

enrolment <- function(measures, rows, institutions, username, password, print = F){
  if(missing(username)){username = getOption("siams.username")}
  if(missing(password)){password = getOption("siams.password")}
  if(missing(print)){print <- FALSE}

  #Hardcoding the values for the arguments
  ##Measures
  `Unique Student Static` <- "[Measures].[Unique Student Static]"
  `Unique Student Current` <- "[Measures].[Unique Student Current]"
  FLE <- "[Measures].[FLE]"
  `Calculated Full-Time for Year` <- "[Measures].[Calculated Full Time for Year]"
  `Calculated Part-Time for Year`  <- "[Measures].[Calculated Part Time for Year]"

  ##Rows
  ###Tombstone
  Gender <- "[Gender].[Gender].[Gender]"
  `Legal Status` <- "[Legal Status].[By Legal Status Group].[Legal Status]"
  `Age Group` <- "[Age Group].[By Age Group].[Age Group]"
  Age <- "[Age Group].[By Age Group].[Age]"

  `Source Country` <- "[Source Country].[Source Country].[Source Country]"
  `Aboriginal Indicator` <- "[Aboriginal Indicator].[By Aboriginal Indicator].[Aboriginal Indicator]"
  `Country Of Citizenship` <- "[Country Of Citizenship].[Country Of Citizenship].[Country Of Citizenship]"
  Language <- "[Language].[Language].[Language]"

  ###Registration
  `Year Of Study` <- "[Year Of Study].[Year Of Study].[Year Of Study]"
  Session <- "[Session].[Session].[Session]"
  `Level Of Study` <- "[Level Of Study].[Level Of Study].[Level Of Study]"
  `Current Status` <- "[Current Status].[Current Status].[Current Status]"
  `Registration Status` <- "[Registration Status].[By Registration Status].[Registration Status]"

  ###Provider
  Provider <- "[Provider].[Provider].[Provider]"
  `Provider Location` <- "[Provider Location].[By Provider and Location].[Provider Location]"
  `Provider Sector` <- "[Provider].[By Current Sector].[Current Sector]"

  ###Service Area
  `Service Area: Country` <- "[Service Area].[By Country and Province].[Country]"
  `Service Area: Province` <- "[Service Area].[By Country and Province].[Province]"

  ###Program
  `Credential Type` <- "[Credential Type].[Credential Type].[Credential Type]"
  `Program Type` <- "[Program Type].[Program Type].[Program Type]"
  `Program Band` <- "[Program Band].[Program Band].[Program Band]"
  `Program Code` <- "[Program and Specialization].[Program Name Code].[Program Name Code]"
  `Program Specialization` <- "[Program and Specialization].[Specialization Name Code].[Specialization Name Code]"
  `Program Length` <- "[Program Length].[Program Length].[Program Length]"
  `CIP Level 4` <- "[CIP Level].[By Four Digits].[Four Digit Level]"
  `CIP Level 6` <- "[CIP Level].[By Six Digits].[Six Digit Level]"
  `CIP Level 2` <- "[CIP Level].[By Two Digits].[Two Digit Level]"

  #source("R/Enrolment_Elements.R", local = T)
  #Setting connection to the cube.
  cnnstr <- paste0("Provider=MSOLAP;
                   Persist Security Info=True;
                   User ID=", username, ";Password=", password,";
                   Initial Catalog=DCAR_DATAMART_PROD;
                   Data Source=https://psdata.eae.alberta.ca/DCaR.datapump;
                   Location=https://psdata.eae.alberta.ca/DCaR.datapump;
                   MDX Compatibility=1;Safety Options=2;MDX Missing Member Mode=Error")
  olapCnn <- OlapConnection(cnnstr)
  qry <- Query()

  #Getting objects
  rows2 <- c()
  for(i in seq_along(rows)){rows2[i] <- get(objects(pattern = rows[i]))}
  measures2 <- c()
  for(i in seq_along(measures)){measures2[i] <- get(objects(pattern = measures[i]))}
  rows_list <- c("Academic Year")
  for(i in seq_along(rows)){rows_list <- c(rows_list, rows[i])}

  #Building the query itself
  ##I've opted to build it so measures and years are always present
  cube(qry) <- "[DCAR Enrol Live Cube]"
  axis(qry, 1) <- c(measures2)
  axis(qry, 2) <- c("NONEMPTY([Academic Year].[Academic Year].[Academic Year].MEMBERS)")

  ##Building the axes
  for(i in seq_along(rows2)){axis(qry, i+2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")}

  #Giving a filter option for provider
  if(missing(institutions)){institutions <- NA}
  providers <- c()
  for(i in seq_along(institutions)){
    if(!is.na(institutions[i])){
      provider <- paste0("[Provider Location].[By Provider].[Provider].&[", institutions[i], "]")
    } else {provider <- NA}
    providers <- c(providers, provider)
  }
  if(is.na(providers[1])){providers <- NA}
  else {providers <- paste(providers, collapse = ", ")}
  providers <- ifelse(is.na(providers[1]), NA, paste0("{", providers, "}"))
  slices <- c("([Registration Status].[By Registration Group].[Registration Group].&[1]",
              providers,
              "[Submission Status].[Submission Status].&[1]",
              "EXCEPT([Registration Type].[Registration Type].[Registration Type].MEMBERS",
              "[Registration Type].[Registration Type].[Registration Type].&[3])",
              "EXCEPT([Legal Status].[By Legal Status].[Legal Status].MEMBERS",
              "[Legal Status].[By Legal Status].[Legal Status].&[5]))") %>%
    na.omit()
  slicers(qry) <- slices

  #Print out MDX string used.
  mdx <- olapR::compose(qry)
  if(print == TRUE){print(mdx)}

  #Execute the query and clean the dataframe.
  df <- executeMD(olapCnn, qry)
  if(is.null(df)){print("Whoops! Something went wrong...")}
  else{
  df <- df %>%
    as_tibble() %>%
    mutate(measure = row.names(.)) %>%
    gather(var, val, -measure) %>%
    mutate(., var = gsub("St. Mary's", "StMary's", var)) %>%
    drop_na(val) %>%
    separate(., var, rows_list[0:length(rows)+1], sep = "\\.") %>%
    unique(.) %>%
    spread(measure, val)

  }
  return(df)
}

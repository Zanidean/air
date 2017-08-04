#'Pull data from ASI
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
#'@param institutions Optional: Filters by Institution
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username"
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password"
#'@param print Optional: Prints the MDX string used to query the database
#'
#'@examples
#' df <- applications(measures = c("Unique Applicant Static", "Application Record Count"),
#'                    rows = c("Provider", "Gender"),
#'                    institutions = c("MH", "MU", "UA"))
#'@export applications

applications <- function(measures, rows, institutions, username, password, print = F){
  #Hardcoding the values for the arguments
  ##Measures
  `Application Record Count` <- "[Measures].[Application Record Count]"
  `Unique Applicant Current` <- "[Measures].[Unique Applicant Current]"
  `Unique Applicant Static` <- "[Measures].[Unique Applicant Static]"


  ##Rows
  ###Tombstone
  Gender <- "[Gender].[Gender].[Gender]"
  `Age Group` <- "[Age Group].[By Applicant Age Group].[Applicant Age Group]"
  Age <- "[Age Group].[By Applicant Age].[Applicant Age]"

  `Aboriginal Indicator` <- "[Aboriginal Indicator].[By Aboriginal Indicator].[Aboriginal Indicator]"

  ###Registration
  Qualified <- "[Qualified].[Qualified].[Qualified]"
  Offered <- "[Offered Admission].[Offered].[Offered]"
  Attending <- "[Attending Application].[Attending].[Attending]"

  `Level Of Study` <- "[Level Of Study].[Level Of Study].[Level Of Study]"
  `Current Status` <- "[Current Status].[Current Status].[Current Status]"
  `Registration Status` <- "[Registration Status].[By Registration Status].[Registration Status]"

  ###Provider
  Provider <- "[Provider].[Provider].[Provider]"
  `Provider Sector` <- "[Provider].[By Current Sector].[Current Sector]"

  # `Service Area: Country Group` <- "[Service Area].[By Service Area].[Country Group]"
  # `Service Area: Province` <- "[Service Area].[By Service Area].[Province]"
  # `Service Area: Postal Code` <- "[Service Area].[By Service Area].[Postal Code]"
  # `Service Area: Service Area` <- "[Service Area].[By Service Area].[Service Area]"
  #

  ###Program
  `Program Name` <- "[Program and Specialization].[Program Name].[Program Name]"
  `Credential Type` <- "[Credential Type].[Credential Type].[Credential Type]"
  `Program Type` <- "[Program Type].[Program Type].[Program Type]"
  `Program Band` <- "[Program Band].[Program Band].[Program Band]"
  `Program Specialization` <- "[Program and Specialization].[Specialization Code And Name].[Specialization Code And Name]"
  `CIP Level 2` <- "[CIP Level].[By Two Digits].[Two Digit Level]"

  if(missing(username)){username = getOption("siams.username")}
  if(missing(password)){password = getOption("siams.password")}

  #source("R/Applications_Elements.R", local = T)
  #Setting connection to the cube.
  cnnstr <- paste0("Provider=MSOLAP;
                    Persist Security Info=True;
                    User ID=", username, ";Password=", password,";
                    Initial Catalog=DCAR_DATAMART_ASI_PROD;
                    Data Source=https://psdata.eae.alberta.ca/DCaR.datapump/ASI;
                    Location=https://psdata.eae.alberta.ca/DCaR.datapump/ASI;
                    MDX Compatibility=1;
                    Safety Options=2;
                    MDX Missing Member Mode=Error")
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
  cube(qry) <- "[DCAR ASI Live Cube]"
  axis(qry, 1) <- c(measures2)
  axis(qry, 2) <- c("NONEMPTY([Academic Year].[Academic Year].[Academic Year].MEMBERS)")

  ##Building the axes
  for(i in seq_along(rows2)){axis(qry, i+2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")}

  #Giving a filter option for provider
  if(missing(institutions)){institutions <- NA}
  providers <- c()
  for(i in seq_along(institutions)){
    if(!is.na(institutions[i])){
      provider <- paste0("[Provider].[By Current Sector].[Provider].&[", institutions[i], "]")
    } else {provider <- NA}
    providers <- c(providers, provider)
  }
  if(is.na(providers[1])){providers <- NA}
  else {providers <- paste(providers, collapse = ", ")}
  providers <- ifelse(is.na(providers[1]), NA, paste0("{", providers, "}, "))
  slices <- paste0("(", providers, "[Submission Status].[Submission Status].&[1])")
  slices <- gsub("NA", "", slices)

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

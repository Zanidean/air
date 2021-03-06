#'Pull Tidy Data from ASI
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
#'@param institutions Optional: Filters by Institution. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param postalcodes Optional: Filters by Postal Code. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param censusdivisions Optional: Filters by Census Division. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param cipcodes Optional: Filters by Cip Code of any length, can used mixed vector with any length cip codes together.
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username"
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password"
#'@param print Optional: Prints the MDX string used to query the database
#'
#'@examples
#' df <- get_applications(measures = c("Unique Applicant Static", "Application Record Count"),
#'                    rows = c("Provider", "Gender"),
#'                    institutions = c("MH", "MU", "UA"))
#'@export get_applications

get_applications <- function(measures, rows,
                             institutions, username, password, print = F,
                             postalcodes, censusdivisions, cipcodes,
                             sa.mh = F){
  #defining a new login function
  getLoginDetails <- function(){
    require(tcltk)
    message("Please use pop-up to authorize.")
    tt <- tcltk::tktoplevel()
    tcltk::tkwm.title(tt, "Get login details")
    Name <- tcltk::tclVar("SIAMS Username")
    Password <- tcltk::tclVar("SIAMS Password")
    entry.Name <- tcltk::tkentry(tt, width = "40", textvariable = Name)
    entry.Password <- tcltk::tkentry(tt, width = "40", show = "*",
                              textvariable = Password)
    tcltk::tkgrid(tcltk::tklabel(tt, text = "Please enter your SIAMS login details."))
    tcltk::tkgrid(entry.Name)
    tcltk::tkgrid(entry.Password)

    OnOK <- function(){
      tcltk::tkdestroy(tt)
    }
    OK.but <- tcltk::tkbutton(tt, text = " Login ", command = OnOK)
    tcltk::tkbind(entry.Password, "<Return>", OnOK)
    tcltk::tkgrid(OK.but)
    tcltk::tkfocus(tt)
    tcltk::tkwait.window(tt)

    invisible(c(loginID = tcltk::tclvalue(Name), password = tcltk::tclvalue(Password)))
  }

  if (missing(username)){
    username <- getOption("siams.username")
    }
  if (missing(password)){
    password <-  getOption("siams.password")
    }
  if (is.null(username) | is.null(password)){
    cred <- getLoginDetails()
    username <- cred[["loginID"]]
    password <- cred[["password"]]
  }
  if (missing(institutions)){
    institutions <- NA
    }
  if (missing(postalcodes)){
    postalcodes <- NA
    }
  if (missing(censusdivisions)){
    censusdivisions <- NA
    }
  if (missing(cipcodes)){
    cipcodes <- NA
    }
  if (missing(sa.mh)){
    sa.mh <- FALSE
    }

  pcs_t <- grepl("exclude", postalcodes) %>% any()
  cds_t <- grepl("exclude", censusdivisions) %>% any()
  cips_t <- grepl("exclude", cipcodes) %>% any()
  providers_t <- grepl("exclude", institutions) %>% any()

  if (sa.mh == T){
    postalcodes <- c("T1A", "T1B", "T1C", "T1R", "T0J", "T0K")
    censusdivisions <- c("1", "2", "4")
  }

  if (missing(rows)){
    rows <- c()
    }
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

  ###Service Area
  `Country` <- "[Service Area].[By Service Area].[Country Group]"
  `Province` <- "[Service Area].[By Service Area].[Province]"
  `Service Area` <- "[Service Area].[By Service Area].[Service Area]"
  `Postal Code` <- "[Service Area].[By Service Area].[Postal Code]"

  ###Program
  `Program Name` <- "[Program and Specialization].[Program Name].[Program Name]"
  `Program Name Code` <- "[Program and Specialization].[Program Code].[Program Code]"
  `Credential Type` <- "[Credential Type].[Credential Type].[Credential Type]"
  `Program Type` <- "[Program Type].[Program Type].[Program Type]"
  `Program Band` <- "[Program Band].[Program Band].[Program Band]"
  `Program Specialization` <- "[Program and Specialization].[Specialization Name].[Specialization Name]"
  `Program Specialization Code` <- "[Program and Specialization].[Specialization Code].[Specialization Code]"
  `CIP Level 2` <- "[CIP Level].[By Two Digits].[Two Digit Level]"
  `CIP Level 4` <- "[CIP Level].[By Two Digits].[Four Digit Level]"
  `CIP Level 6` <- "[CIP Level].[By Two Digits].[Six Digit Level]"

  #Setting connection to the cube.
  cnnstr <- paste0("Provider=MSOLAP;
                    Persist Security Info=True;
                    User ID=", username, ";Password=", password, ";
                    Initial Catalog=DCAR_DATAMART_ASI_PROD;
                    Data Source=https://PSData.ae.alberta.ca/DCaR.datapump/ASI;
                    Location=https://PSData.ae.alberta.ca/DCaR.datapump/ASI;
                    MDX Compatibility=1;
                    Safety Options=2;
                    MDX Missing Member Mode=Error")
  olapCnn <- OlapConnection(cnnstr)

  qry <- Query()

  #Getting objects
  rows2 <- c()
  for (i in seq_along(rows)){
    rows2[i] <- get(objects(pattern = rows[i]))
    }
  measures2 <- c()
  for (i in seq_along(measures)){
    measures2[i] <- get(objects(pattern = measures[i]))
    }
  rows_list <- c("Academic Year")
  for (i in seq_along(rows)){
    rows_list <- c(rows_list, rows[i])
    }

  #Building the query itself
  ##I've opted to build it so measures and years are always present
  cube(qry) <- "[DCAR ASI Live Cube]"

  axis(qry, 1) <- c(measures2)

  axis(qry, 2) <- c("NONEMPTY([Academic Year].[Academic Year].[Academic Year].MEMBERS)")

  ##Building the axes
  for (i in seq_along(rows2)){
    axis(qry, i + 2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")
    }

  fltr <- function(arg, place){
    arg <- na.omit(arg)
    arg <- arg[arg != "exclude"]
    if (missing(arg)){
      arg <- NA
      }
    if (!is.na(arg[1])){
      output <- paste(place, arg, "]",
                      sep = "", collapse = ", ") %>%
        paste0("{", ., "},")
    } else {
      output <- NA
      }
    return(output)
  }

  #Giving a filter option for provider
  providers <- fltr(institutions, "[Provider].[By Current Sector].[Provider].&[")

  #Giving a filter option for postalcodes
  pcs <- fltr(postalcodes, "[Service Area].[By Service Area].[Postal Code].&[CAME")

  #Giving a filter option for census division in service area
  cds <- fltr(censusdivisions, "[Census Divsion].[By Census Division].[Census Division].&[0")
  #filter for cipcodes

  filtercips <- function(sub, place){
    if (length(sub) > 0){
      output <- c()
      for (i in seq_along(sub)){
        if (!is.na(sub[i])){
          output_part <- paste0(place, sub[i], "]")
        } else {
          output_part <- NA
          }
        output <- c(output, output_part)
      }
      if (is.na(output[1])){
        output <- NA
      } else {
          output <- paste(output, collapse = ", ")
          }
    } else {
      output <- NA
      }
    return(output)
  }
  cips_2 <- cipcodes[str_length(cipcodes) == 2]
  cips_4 <- cipcodes[str_length(cipcodes) == 5]
  cips_6 <- cipcodes[str_length(cipcodes) == 7]
  cips2 <- filtercips(cips_2, "[CIP Level].[By Two Digits].[Two Digit Level].&[")
  cips4 <- filtercips(cips_4, "[CIP Level].[By Two Digits].[Four Digit Level].&[")
  cips6 <- filtercips(cips_6, "[CIP Level].[By Two Digits].[Six Digit Level].&[")
  cips <- c(cips2, cips4, cips6)
  cips <- cips[!is.na(cips)]
  cips <- paste(cips, collapse = ", ")
  cips <- ifelse(is.na(cips[1]), NA, paste0("{", cips, "},"))
  cips <- ifelse(cips %in% c("{}", "{},"), NA, cips)

  excluder <- function(input, test, place){
    if (test == T){
      output <- gsub("},", "}", input)
      output <- paste0("EXCEPT(", place, ".MEMBERS, ", output, "), ")
    } else (output <- input)
    return(output)
  }


  pcs <- excluder(pcs, pcs_t, "[Service Area].[By Service Area].[Postal Code]")
  cds <- excluder(cds, cds_t, "[Census Division].[By Census Division].[Census Division]")
  providers <- excluder(providers, providers_t, "[Provider].[By Current Sector].[Provider]")

  #applying filters
  slices <- paste0("(", providers, pcs, cds, cips,
                   "[Submission Status].[Submission Status].&[1])")
  slices <- gsub("NA", "", slices)

  slicers(qry) <- slices

  #Print out MDX string used.
  mdx <- olapR::compose(qry)



  if (print == TRUE){
    print(mdx)
    }
  #Execute the query and clean the dataframe.
  df <- executeMD(olapCnn, mdx)

  if (is.null(df)){
    message("Whoops! Something went wrong...")
    }
  else{
    df <- df %>%
      as.data.frame()
    df$measure <- row.names(df)
    df <- reshape2::melt(df, id.vars = "measure") %>%
      as_tibble() %>%
      mutate(., variable = gsub("St. Mary's", "StMary's", variable)) %>%
      group_by(measure, variable) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      filter(value > 0) %>%
      spread(measure, value) %>%
      mutate(variable = variable %>% str_replace("Division No.", "Division Number")) %>%
      mutate(
        variable = str_replace(
          variable,
          "(\\.[0-9][0-9]\\.)",
          str_extract(variable, "(\\.[0-9][0-9])")
        ) %>%
          str_replace("\\.\\,", "\\,") %>%
          str_replace("\\.\\,", "\\,")
      )%>%
      separate(., variable, rows_list[0:length(rows) + 1], sep = "\\.")
  }
  return(df)
}

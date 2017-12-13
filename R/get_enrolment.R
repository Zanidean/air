#'Pull data from LERS
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
#'@param institutions Optional: Filters by Institution. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param postalcodes Optional: Filters by Postal Code. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param censusdivisions Optional: Filters by Census Division. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param cipcodes Optional: Filters by Cip Code of any length, can used mixed vector with any length cip codes together.
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@param print Optional: Prints the MDX string used to query the database.
#'
#'@examples
#' df <- get_enrolment(measures = c("Unique Student Static", "FLE"),
#'                 rows = c("Provider", "Gender"),
#'                 institutions = c("MH", "MU", "UA"),
#'                 postalcodes = c("exclude", "T1C"),
#'                 censusdivision = c("exclude", "1"))
#'@export get_enrolment

get_enrolment <- function(measures, rows, institutions, username, password,
                           print = F, remove.offshores = T,
                           remove.continuingstudies = T,
                           postalcodes,
                           censusdivisions,
                           cipcodes,
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
    OK.but <-tcltk::tkbutton(tt, text = " Login ", command = OnOK)
    tcltk::tkbind(entry.Password, "<Return>", OnOK)
    tcltk:: tkgrid(OK.but)
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

  if (missing(print)){
    print <- FALSE
    }
  if (missing(remove.offshores)){
    offshores <- TRUE
    }
  if (missing(remove.continuingstudies)){
    continuingstudies <- TRUE
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

  if(sa.mh == T){
    postalcodes <-  c("T1A", "T1B", "T1C", "T1R", "T0J", "T0K")
    censusdivisions <-  c("1", "2", "4")
  }

  if (missing(rows)){
    rows <- c()
    }
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
  `Grade Completed Year` <- "[Grade Completed Year].[By Grade Completed Year].[Grade Completed Year]"
  `Current Status` <- "[Current Status].[Current Status].[Current Status]"

  ###Registration
  `Year Of Study` <- "[Year Of Study].[Year Of Study].[Year Of Study]"
  Session <- "[Session].[Session].[Session]"
  `Level Of Study` <- "[Level Of Study].[Level Of Study].[Level Of Study]"
  `Current Status` <- "[Current Status].[Current Status].[Current Status]"
  `Registration Status` <- "[Registration Status].[By Registration Status].[Registration Status]"
  `Registration Type` <- "[Registration Type].[Registration Type].[Registration Type]"
  `Level of Study` <- "[Level Of Study].[Level Of Study].[Level Of Study]"
  `Calculated Learned Registration Status` <- "[Calculated Learner Registration Status].[Calculated Learner Registration Status].[Calculated Learner Registration Status]"
  `Attainment` <- "[Attainment].[Attainment].[Attainment]"

  ###Provider
  Provider <- "[Provider].[Provider].[Provider]"
  `Provider Location` <- "[Provider Location].[By Provider and Location].[Provider Location]"
  `Provider Sector` <- "[Provider].[By Current Sector].[Current Sector]"
  `Provider Service Area` <- "[Provider].[By Service Area].[Service Area]"


  ###Service Area
  `Country` <- "[Service Area].[By Country and Province].[Country]"
  `Province` <- "[Service Area].[By Country and Province].[Province]"
  `Service Area` <- "[Service Area].[By Country and Province].[Service Area]"
  `Postal Code` <- "[Service Area].[By Country and Province].[Postal Code]"
  `Census Division` <- "[Census Division].[By Country and Province].[Census Division]"
  ###Program
  `Credential Type` <- "[Credential Type].[Credential Type].[Credential Type]"
  `Program Type` <- "[Program Type].[Program Type].[Program Type]"
  `Program Band` <- "[Program Band].[Program Band].[Program Band]"
  `Program Name` <- "[Program and Specialization].[Program Name].[Program Name]"
  `Program Name Code` <- "[Program and Specialization].[Program Code].[Program Code]"
  `Program Specialization` <- "[Program and Specialization].[Specialization Name].[Specialization Name]"
  `Program Specialization Code` <- "[Program and Specialization].[Specialization Code].[Specialization Code]"
  `Program Length` <- "[Program Length].[Program Length].[Program Length]"
  `CIP Level 4` <- "[CIP Level].[By Four Digits].[Four Digit Level]"
  `CIP Level 6` <- "[CIP Level].[By Six Digits].[Six Digit Level]"
  `CIP Level 2` <- "[CIP Level].[By Two Digits].[Two Digit Level]"

  #Setting connection to the cube.
  cnnstr <- paste0("Provider=MSOLAP;
                   Persist Security Info=True;
                   User ID=", username, ";Password=", password, ";
                   Initial Catalog=DCAR_DATAMART_PROD;
                   Data Source=https://psdata.eae.alberta.ca/DCaR.datapump;
                   Location=https://psdata.eae.alberta.ca/DCaR.datapump;
                   MDX Compatibility=1;Safety Options=2;MDX Missing Member Mode=Error")
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
  cube(qry) <- "[DCAR Enrol Live Cube]"
  axis(qry, 1) <- c(measures2)
  axis(qry, 2) <- c("NONEMPTY([Academic Year].[Academic Year].[Academic Year].MEMBERS)")

  ##Building the axes
  for (i in seq_along(rows2)){
    axis(qry, i + 2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")
    }


  if (remove.continuingstudies == T){
    rm_cs <- c("EXCEPT([Registration Type].[Registration Type].[Registration Type].MEMBERS",
               "[Registration Type].[Registration Type].[Registration Type].&[3])")
  } else {
    rm_cs <- NA
    }

  if (remove.offshores == T){
    rm_os <- c("EXCEPT([Legal Status].[By Legal Status].[Legal Status].MEMBERS",
               "[Legal Status].[By Legal Status].[Legal Status].&[5])")
  } else {
    rm_os <- NA
    }

  fltr <- function(arg, place){
    arg <- na.omit(arg)
    arg <- arg[arg != "exclude"]
    if (missing(arg)){
      arg <- NA
      }
    if (!is.na(arg[1])){
      output <- paste(place,
                      arg, "]",
                      sep = "", collapse = ", ") %>%
        paste0("{", ., "}")
    } else {
      output <- NA
      }
    return(output)
  }

  #Giving a filter option for provider
  providers <- fltr(institutions, "[Provider Location].[By Provider].[Provider].&[")
  #Giving a filter option for postalcodes in service area
  pcs <- fltr(postalcodes, "[Service Area].[By Country and Province].[Postal Code].&[CAME")
  #Giving a filter option for postalcodes in service area
  cds <- fltr(censusdivisions, "[Census Division].[By Country and Province].[Census Division].&[0")

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
  cips <- ifelse(is.na(cips[1]), NA, paste0("{", cips, "}"))
  cips <- ifelse(cips == "{}", NA, cips)

  excluder <- function(input, test, place){
    if (test == T){
      output <- gsub("},", "}", input)
      output <- paste0("EXCEPT(", place, ".MEMBERS, ", output, ")")
    } else (output <- input)
    return(output)
  }

  pcs <- excluder(pcs, pcs_t,
                  "[Service Area].[By Country and Province].[Postal Code]")
  cds <- excluder(cds, cds_t,
                  "[Census Division].[By Country and Province].[Census Division]")

  providers <- excluder(providers, providers_t,
                        "[Provider Location].[By Provider].[Provider]")

  slices <- c("([Registration Status].[By Registration Group].[Registration Group].&[1]",
              providers, pcs, cds, cips, rm_cs, rm_os, "[Submission Status].[Submission Status].&[1])") %>%
    na.omit()
  slicers(qry) <- slices

  #Print out MDX string used.
  mdx <- olapR::compose(qry)
  if (print == TRUE){
    print(mdx)
    }

  #Execute the query and clean the dataframe.
  df <- executeMD(olapCnn, qry)
  if (is.null(df)){
    message("Whoops! Something went wrong...")
    }
  else {
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
      separate(., variable, rows_list[0:length(rows) + 1], sep = "\\.")
  }
  return(df)
}

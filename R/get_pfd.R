#'Pull Tidy Data from PFD
#'@param measures Select possible measurements
#'@param rows Optional: Select rows to cut data by
#'@param institutions Optional: Filters by Institution. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@param print Optional: Prints the MDX string used to query the database.
#'
#'@examples
#' df <- get_pfd("Cost per FLE", "Program Band", "MH")

#'@export get_pfd
#'

get_pfd <-
  function(measures,
           rows,
           institutions,
           username,
           password,
           print = F) {
    #defining a new login function

    require(olapR)
    require(dplyr)
    require(tidyr)
    require(stringr)

    getLoginDetails <- function() {
      require(tcltk)
      message("Please use pop-up to authorize.")
      tt <- tcltk::tktoplevel()
      tcltk::tkwm.title(tt, "Get login details")
      Name <- tcltk::tclVar("SIAMS Username")
      Password <- tcltk::tclVar("SIAMS Password")
      entry.Name <-
        tcltk::tkentry(tt, width = "40", textvariable = Name)
      entry.Password <- tcltk::tkentry(tt,
                                       width = "40",
                                       show = "*",
                                       textvariable = Password)
      tcltk::tkgrid(tcltk::tklabel(tt, text = "Please enter your SIAMS login details."))
      tcltk::tkgrid(entry.Name)
      tcltk::tkgrid(entry.Password)

      OnOK <- function() {
        tcltk::tkdestroy(tt)
      }
      OK.but <- tcltk::tkbutton(tt, text = " Login ", command = OnOK)
      tcltk::tkbind(entry.Password, "<Return>", OnOK)
      tcltk::tkgrid(OK.but)
      tcltk::tkfocus(tt)
      tcltk::tkwait.window(tt)

      invisible(c(
        loginID = tcltk::tclvalue(Name),
        password = tcltk::tclvalue(Password)
      ))
    }
    if (missing(username)) {
      username <- getOption("siams.username")
    }
    if (missing(password)) {
      password <-  getOption("siams.password")
    }
    if (is.null(username) | is.null(password)) {
      cred <- getLoginDetails()
      username <- cred[["loginID"]]
      password <- cred[["password"]]
    }

    if (missing(print)) {
      print <- FALSE
    }

    if (missing(institutions)) {
      institutions <- NA
    }
    providers_t <- grepl("exclude", institutions) %>% any()
    if (missing(rows)) {
      rows <- c()
    }

    warning("If you're reading this warning, get_pfd is still in development. Try the Excel cubes provided by the ministry if you need confident and accurate information.")

    # Measures
    `Cost per FLE` <- "[Measures].[Cost per FLE]"
    `Total Program Cost` <- "[Measures].[Total Program Cost]"
    `Direct Cost` <- "[Measures].[Direct Cost]"
    `Indirect Cost` <- "[Measures].[Indirect Cost]"
    `Record Count` <- "[Measures].[Record Count]"
    `Full Load Equivalent` <- "[Measures].[FLE]"
    All <- "[Measures].AllMembers"

    # Rows
    `CIP Level 2` <- "[Cip Code].[By Two Digits].[Two Digit Level]"
    `CIP Level 6` <- "[Cip Code].[By Six Digits].[Six Digit Level]"
    `CIP Level 4` <- "[Cip Code].[By Four Digits].[Four Digit Level]"

    `Level Of Study` <- "[Level Of Study].[Level Of Study].[Level Of Study]"
    `Credential Type` <- "[Credential Type].[Credential Type].[Credential Type]"
    `Program Band` <- "[Program Band].[Program Band].[Program Band]"
    `Program Length` <- "[Program Length].[Program Length].[Program Length]"
    `Program Type` <- "[Program Type].[Program Type].[Program Type]"

    `Program Name` <- "[Program Specialization].[By Program].[Program]"
    `Program Specialization Name` <- "[Program Specialization].[By Program].[Specialization]"
    `Program Code` <- "[Program Specialization].[Program Code].[Program Code]"
    `Program Specialization Code` <- "[Program Specialization].[Specialization Code].[Specialization Code]"
    `Provider` <- "[Provider].[By Current Sector].[Provider]"

    `STEM and BHASE` <- "[STEM And BHASE].[By STEM And BHASE].[STEM And BHASE]"
    #`STEM and BHASE Subgroups` <- "[STEM And BHASE].[By STEM And BHASE].[STEM And BHASE Subgroups]"
    #`STEM and BHASE Categories` <- "[STEM And BHASE].[By STEM And BHASE].[STEM And BHASE Categories]"

    #Setting connection to the cube.
    cnnstr <- paste0(
      "Provider=MSOLAP;
      Persist Security Info=True;
      User ID=",
      username,
      ";Password=",
      password,
      ";
      Initial Catalog=DCAR_PFD_DM_PROD;
      Data Source=https://PSData.ae.alberta.ca/DCaR.datapump/PFD;
      Location=https://PSData.ae.alberta.ca/DCaR.datapump/PFD;
      MDX Compatibility=1;Safety Options=2;MDX Missing Member Mode=Error"
    )

    olapCnn <- OlapConnection(cnnstr)
    qry <- Query()


    #Getting objects
    rows2 <- c()
    for (i in seq_along(rows)) {
      rows2[i] <- get(objects(pattern = rows[i]))
    }
    measures2 <- c()
    for (i in seq_along(measures)) {
      measures2[i] <- get(objects(pattern = measures[i]))
    }
    rows_list <- c("Fiscal Year")
    for (i in seq_along(rows)) {
      rows_list <- c(rows_list, rows[i])
    }

    #Building the query itself
    ##I've opted to build it so measures and years are always present
    cube(qry) <- "[PFD Live Cube]"
    axis(qry, 1) <- c(measures2)
    axis(qry, 2) <-
      c("NONEMPTY([Fiscal Year].[Fiscal Year].[Fiscal Year].MEMBERS)")

    ##Building the axes
    for (i in seq_along(rows2)) {
      axis(qry, i + 2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")
    }

    # Filter to select only institutions
    fltr <- function(arg, place) {
      arg <- na.omit(arg)
      arg <- arg[arg != "exclude"]
      if (missing(arg)) {
        arg <- NA
      }
      if (!is.na(arg[1])) {
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
    providers <- fltr(institutions, "[Provider].[By Provider].[By Provider].&[")


    # Exclude list of institutions
    excluder <- function(input, test, place) {
      if (test == T) {
        output <- gsub("},", "}", input)
        output <- paste0("EXCEPT(", place, ".MEMBERS, ", output, ")")
      } else
        (output <- input)
      return(output)
    }

    providers <- excluder(providers,
                          providers_t,
                          "[Provider Location].[By Provider].[Provider]")


    slices <-
      c("([Submission Cycle Status].[Submission Cycle Status].&[2]",
        providers,
        "[Submission Status].[Submission Status].&[1])"
      ) %>%
      na.omit()
    slicers(qry) <- slices

    #Print out MDX string used.
    mdx <- olapR::compose(qry)
    if (print == TRUE) {
      print(mdx)
    }

    #Execute the query and clean the dataframe.
    df <- executeMD(olapCnn, qry)
    if (is.null(df)) {
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
        mutate(variable = variable %>% str_replace("Division No.", "Division Number")) %>%
        mutate(
          variable = str_replace(
            variable,
            "(\\.[0-9][0-9]\\.)",
            str_extract(variable, "(\\.[0-9][0-9])")
          ) %>%
            str_replace_all("\\.\\,", "\\,") %>%
            str_replace_all("\\.\\,", "\\,")
        ) %>%
        separate(., variable, rows_list[0:length(rows) + 1], sep = "\\.") %>%
        mutate(`Fiscal Year` = `Fiscal Year` %>% str_replace("-20", "-") %>% str_replace(" - 20", "-"))
    }
    return(df)
}

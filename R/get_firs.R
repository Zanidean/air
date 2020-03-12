#'Pull Tidy Data from FIRS
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
#'@param institutions Optional: Filters by Institution. Can exclude by including "exclude" in the vector, then the items in that vector will be excluded rather than included.
#'@param username Optional: Either supply a siams username or use .Rprofile otherwise as "siams.username".
#'@param password Optional: Either supply a siams password or use .Rprofile otherwise as "siams.password".
#'@param print Optional: Prints the MDX string used to query the database.
#'
#'@examples
#' df <- get_firs(measures = "value",
#'                rows = c("Provider", "Category A Level 2", "Catergory B Level 3"),
#'                institions = "Medicine Hat College")
#'@export get_firs

get_firs <-
  function(measures = "Value",
           rows,
           institutions,
           username,
           password,
           print = F) {

    require(olapR)
    require(dplyr)
    require(tidyr)
    require(stringr)

    #defining a new login function
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

    if (missing(rows)) {
      rows <- c()
    }

    if (missing(institutions)) {
      institutions <- NA
    }
    providers_t <- grepl("exclude", institutions) %>% any()

    `Value` <- "[Measures].[Value]"
    `Record Count` <- "[Measures].[Record Count]"
    All <- "[Measures].AllMembers"

    `Record Type` <- "[Record Type].[Record Type].[Record Type]"

    Provider <- "[Provider].[By Current Sector].[Provider]"
    `Sector` <- "[Provider].[By Current Sector].[Current Sector]"
    `Previous Sector` <- "[Provider].[By Previous Sector].[Previous Sector]"

    `Category A Level 2` <- "[Category A].[Category A].[Level 02]"
    `Category A Level 3` <- "[Category A].[Category A].[Level 03]"
    `Category B Level 2` <- "[Category B].[Category B].[Level 02]"
    `Category B Level 3` <- "[Category B].[Category B].[Level 03]"
    `Category B Level 4` <- "[Category B].[Category B].[Level 04]"

    `Template` <- "[Template].[Template].[Template]"


    #Setting connection to the cube.
    cnnstr <- paste0(
      "Provider=MSOLAP;
      Persist Security Info=True;
      User ID=",
      username,
      ";Password=",
      password,
      ";
      Initial Catalog=DCAR_FIRS_DM_PROD;
      Data Source=https://PSData.ae.alberta.ca/DCaR.datapump/FIRS;
      Location=https://PSData.ae.alberta.ca/DCaR.datapump/FIRS;
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
    cube(qry) <- "[DCAR FIRS Live Cube]"
    axis(qry, 1) <- c(measures2)
    axis(qry, 2) <-
      c("NONEMPTY([Fiscal Year].[Fiscal Year].[Fiscal Year].MEMBERS)")

    ##Building the axes
    for (i in seq_along(rows2)) {
      axis(qry, i + 2) <- paste0("NONEMPTY(", rows2[i], ".MEMBERS)")
    }

    # filter data
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
    providers <-
      fltr(institutions,
           "[Provider].[By Provider].[By Provider].&[")


    slices <-
      c(
        "([Submission Cycle Status].[Submission Cycle Status].&[2]",
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
        mutate(variable = variable %>% str_replace("Non.Academic Support", "Non-Academic Support")) %>%
        mutate(variable = variable %>% str_replace("Cond. Grants", "Cond. Grants")) %>%
        mutate(variable = variable %>% str_replace("Non.Refundable", "Non-Refundable ")) %>%
        mutate(variable = variable %>% str_replace("MFP Exp. Not Subj", "MFP Exp Not Subj")) %>%
        mutate(variable = variable %>% str_replace("Misc. Fees", "Misc Fees")) %>%
        mutate(
          variable = str_replace(
            variable,
            "(\\.[0-9][0-9]\\.)",
            str_extract(variable, "(\\.[0-9][0-9])")
          ) %>%
            str_replace("\\.\\,", "\\,") %>%
            str_replace("\\.\\,", "\\,")
        ) %>%
        separate(., variable, rows_list[0:length(rows) + 1], sep = "\\.") %>%
        mutate(`Fiscal Year` = `Fiscal Year` %>% str_replace("-20", "-"))%>%
        mutate(`Fiscal Year` = `Fiscal Year` %>% str_replace(" - 20", "-"))
    }
    return(df)

  }














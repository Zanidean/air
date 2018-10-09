#'Pull Tidy Data from PFD
#'@param measures Select possible measurements
#'@param rows Select rows to cut data by
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

    warning("If you're reading this warning, get_pfd is still in development. Try the Excel cubes provided by the ministry. ")



    # Measures

    # Rows



    # CnnString



}

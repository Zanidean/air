#'Get filter codes to supply to filtering arguments
#'@param code Which argument would you like to see filters for?
#'@examples air_codes("institions")
#'@export air_codes
air_codes <- function(arg = "institutions") {
  if (arg == "institutions") {
    insts = c(
      "Medicine Hat College" = "MH",
      "Alberta College of Art and Design" = "AA",
      "Ambrose University College" = "CA",
      "Athabasca University" = "AU",
      "Augustana Faculty of the U of A" = "CL",
      "Bow Valley College" = "VC",
      "Canadian Nazarene University College" = "CN",
      "Canadian University College" = "CU",
      "Concrodia University College of Alberta" = "CC",
      "Grand Prairie Regional College" = "GP",
      "Grant MacEwan College" = "GM",
      "Grant MacEwan University" = "GU",
      "Keyano College" = "KC",
      "King's University College" = "KK",
      "Lakeland College" = "LL",
      "Lethbridge College" = "LC",
      "Mount Royal University" = "MU",
      "Mount Royal College" = "MR",
      "Norquest College" = "VE",
      "Northern Alberta Institute of Technology" = "NA",
      "Northern lakes College" = "VS",
      "Olds College" = "OC",
      "Portage College" = "VB",
      "Red Deer College" = "RD",
      "Southern Alberta Institute of Technology" = "SA",
      "St.Mary's University College" = "SM",
      "Taylor University College and Seminary" = "NB",
      "University of Alberta" = "UA",
      "University of Calgary" = "UC",
      "University of Lethbridge" = "UL"
    )

    message("Institution Codes")
    print(insts)
  } else
    (message("Invalid Argument"))
}

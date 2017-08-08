Overview
---------

air is a package for pulling tidy data frames from the DCARS databases
=======
`enrolment()`: Pulls data from the LERS Live 

`applications()`: Pulls data form the ASI Live datacube

`dcars()`: Uses both functions to create a dataframe of both enrolments and applicants

Installation
------------
``` R
install.packages(devtools)
devtools::install_bitbucket("ecortens/air", auth_user = "", password = "")
```

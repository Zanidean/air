Overview
---------

air is a package for pulling tidy data frames from DCARS
=======
`get_enrolment()`: Pulls data from the LERS live cube

`get_applications()`: Pulls data form the ASI live cube

`get_dcars()`: Uses both functions to create a dataframe of both enrolments and applicants

Installation
------------
``` R
install.packages(devtools)
devtools::install_bitbucket("ecortens/air", auth_user = "", password = "")
```

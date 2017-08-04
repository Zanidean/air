Overview
---------

<<<<<<< HEAD
air is a package for pulling tidy dataframes from the DCARS databases
=======
air is a package for pulling tidy frames from the DCARS databases
>>>>>>> fdb71fe743850f2b58c09f7c3d3a1a6f5e4d87af

- `enrolment()`: Pulls data from the LERS Live datacube

- `applications()`: Pulls data form the ASI Live datacube

- `dcars()`: Uses both functions to create a dataframe of both enrolments and applicants


Installation
------------
``` R
install.packages(devtools)
devtools::install_bitbucket("ecortens/air", auth_user = "", password = "")
```

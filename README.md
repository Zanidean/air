Overview
---------

air is a package for pulling tidy data frames from LERS and ASI

`get_enrolment()`: Pulls data from the LERS live cube

`get_applications()`: Pulls data form the ASI live cube

`get_dcars()`: Uses both functions to create a dataframe of both enrollments and applicants

`get_rate()`: Uses ASI cube to automatically calculate the conversion or acceptance rates

`air_vars()`: Returns a list of all exact strings to put into the above functions.


Installation
------------

Installing air is not as simple as it may seem. This package depends on some parts of Microsoft R and a SQL service.

These installation instructions can be found [here](https://docs.microsoft.com/en-us/machine-learning-server/r-client/install-on-windows).

Once you've got that all sorted out, you'll be able to easily install air. The latest version can be installed like this:

``` R
install.packages(devtools)
devtools::install_bitbucket("ecortens/air", auth_user = "", password = "")
```

Usage
------------

If you've used the Excel templates provided by the ministry, then you know the frustration of force closes, long waits, and difficult reproducibility - air is meant to help out with these problems. 

The syntax of air attempts to get as close to a natural language request as is possible, while making some assumptions about how you'd like to structure that request.

You may request *"Please pull FLE enrolment for Medicine Hat College by Program"*

Using air, that query would look like this:
``` R
get_enrolment(measures = "FLE",
              rows = "Program Name",
              institutions = "MH")
```

There are some assumptions this call makes, such as authentication credentials being stored as an option R can pull from, and that you want to use the common MHC filters. 

Some of these assumptions can be easily reverted:
``` R
get_enrolment(measures = "FLE",
              rows = "Program Name", 
              institutions = "MH", 
              remove.offshores = F)
```

And we can apply more MHC specific filters just as easily. 

This section filters to just include enrolment from our service area:
``` R
get_enrolment(measures = "FLE",
              rows = "Program Name", 
              institutions = "MH", 
              sa.mh = T)
```

For information about errors and problems, check the [problems and solutions file.](file:///Q:/StrategicResearch/Rules%20and%20Procedures%20Folder/Checklists%20and%20procedures/Procedure%20Manual/Data%20Analysis/Pages/issues.html#air)

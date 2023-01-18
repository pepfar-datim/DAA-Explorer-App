# DAA-R-Shiny

**Repo Owner:** Vlad Shioshvili [@vshioshvili](https://github.com/vshioshvili), backup: Jason Knueppel [@jknuep](https://github.com/jknuep)

**Repo Owner:** Jason Knueppel [\@jknuep](https://github.com/jknuep)

## Running The R Shiny App

Ensure that you have configured the appropriate environment variables before running your R Shiny app. Below is an example of all the environment variables set up with the use of an `.Rprofile` file:

    Sys.setenv(DATIM_URL = "https://www.datim.org/")
    Sys.setenv(GEOALIGN_URL = "https://geoalign.datim.org/")
    Sys.setenv(GEOALIGN_USERNAME = "XXXXXXXXXXXXXXXXXXX")
    Sys.setenv(GEOALIGN_PASSWORD = "XXXXXXXXXXXXXXXX")
    Sys.setenv(AWS_S3_BUCKET = "prod.pepfar.data.raw")
    Sys.setenv(AWS_ACCESS_KEY_ID = "XXXXXXXXXXXXXXXXXXXX")
    Sys.setenv(AWS_SECRET_ACCESS_KEY = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    Sys.setenv(AWS_REGION = "us-east-2")
    Sys.setenv(LOG_PATH = "daa-analysis.log")

The variables should include:

- The instances of DATIM (`DATIM_URL`) and GeoAlign (`GEOALIGN_URL`) that the app should communicate with (e.g. production, test, etc.).
- A username (`GEOALIGN_USERNAME`) and password (`GEOALIGN_PASSWORD`) for the app to use to sign-in to the specified instance of GeoAlign.
- Address and credential for the app to access data stored on the S3 Data Lake (`AWS_S3_BUCKET`, `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_REGION`)
- The filepath where the app should save logs (`LOG_PATH`)

If you are deploying the app to RStudio Connect, please refer to the RStudio Connect User Manual (https://docs.rstudio.com/connect/user/content-settings/#content-vars).

Have a question? Find us on [GitHub](https://github.com/pepfar-datim/DAA-R-Shiny/issues/new) or [DATIM Support](https://datim.zendesk.com) (DATIM users only).

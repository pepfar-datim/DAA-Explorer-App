# Import packages -----------------------------------------------------
## Shiny packages -------------
library(shiny, warn.conflicts = F, quietly = T)
library(shinyWidgets, warn.conflicts = F, quietly = T)
# library(thematic, warn.conflicts = F, quietly = T)
# library(bslib, warn.conflicts = F, quietly = T)

## Shiny add-ons --------------
# library(shinydashboard, warn.conflicts = F, quietly = T)
library(htmlwidgets, warn.conflicts = F, quietly = T)
library(rpivotTable, warn.conflicts = F, quietly = T)
library(shinyjs, warn.conflicts = F, quietly = T)

## Tidyverse packages ---------
library(magrittr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(stringr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(tidyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(rvest, warn.conflicts = F, quietly = T)
library(glue, warn.conflicts = F, quietly = T)

## Visualization packages -----
library(ggrepel, warn.conflicts = F, quietly = T)
library(viridis, warn.conflicts = F, quietly = T)
library(scales, warn.conflicts = F, quietly = T)
library(plotly, warn.conflicts = F, quietly = T)
library(DT, warn.conflicts = F, quietly = T)
library(gt, warn.conflicts = F, quietly = T)

## DATIM packages -------------
library(daa.analytics, warn.conflicts = F, quietly = T)
library(datimutils, warn.conflicts = F, quietly = T)
# library(datapackr, warn.conflicts = F, quietly = T)

## Additional tools -----------
library(futile.logger, warn.conflicts = F, quietly = T)
library(openxlsx, warn.conflicts = F, quietly = T)
library(paws, warn.conflicts = F, quietly = T)

# Load local functions -----------------------------------
source("R/fetch.R")
source("R/utils.R")
source("R/visuals.R")
source("R/ui_main_module.R")
source("R/ui_login_module.R")
source("R/ui_unauth_module.R")

# App Information -----------------------------------------
banner_message <-
  paste0('<h3 style="text-align:center;"><strong>IMPORTANT NOTICE</strong></h3>',
         '<br><ul><li>MOH Data Alignment Data is now available for the FY21 ',
         'Activity. This data can be seen within the `Site Data` tab, ',
         'the `Pivot Table` tab, and within files from the `Downloads` ',
         'tab.</li><li>PEPFAR data for FY2021 will not be available until ',
         'after the close of the MER Data Import window (Nov 12) and will ',
         'not populate in the app until sometime the following week. ',
         '<strong>Until that time, no concordance data can be shown inside ',
         'the app.</strong></li></ul><br>')

app_info <- list(version = "2021.2.0",
                 message = banner_message)
wiki_url <- a("Data Alignment Support Site",
              href = glue::glue("
                               https://datim.zendesk.com/hc/en-us/categories/\\
                               360000927432-PEPFAR-MoH-Data-Alignment-Activity
                               "),
              target = "_blank")

# Dropdown Information --------------------------
filter_options <- list(de_list = get_indicator_list(),
                       pe_list = get_period_list(),
                       integrity_list =
                         c(`Missing MOH IDs` = "integrity_nulls",
                           `Duplicate MOH IDs` = "integrity_duplicates"),
                       integrity_selected = "integrity_nulls")

# Setting Theme Variables ---------------------------------
options("scipen" = 999)
my_theme <- "cosmo"

# App Information -----------------------------------------
source("./R/fetch.R")
source("./R/ui_login_module.R")
source("./R/ui_main_module.R")
source("./R/ui_unauth_module.R")
source("./R/utilities.R")
source("./R/visuals.R")
app_info <- list(version = "v1.1.0")
wiki_url <- a("Data Alignment Support Site",
              href = glue::glue("
                               https://datim.zendesk.com/hc/en-us/categories/\\
                               360000927432-PEPFAR-MoH-Data-Alignment-Activity
                               "),
              target = "_blank")
last_updated <- list(updated_last = daa.analytics::get_last_modified())

# Dropdown Information --------------------------
filter_options <- list(de_list = get_indicator_list(),
                       pe_list = get_period_list(),
                       integrity_list =
                         c(`Missing MOH IDs` = "integrity_nulls",
                           `Duplicate MOH IDs` = "integrity_duplicates"),
                       integrity_selected = "integrity_nulls")

# Setting Theme Variables ---------------------------------
options("scipen" = 999)

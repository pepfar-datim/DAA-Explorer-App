# Configuration ---------------------------------------------------------------

#Initiate logging
logger <- futile.logger::flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

futile.logger::flog.appender(futile.logger::appender.console(),
                             name = "daa-analysis")

# User Access Information ---------------------------------------------------

#' Check User Credentials
#'
#' @description Checks user groups and user roles to find if a user
#' is either a global user, superuser, or has MoH-specific read access for
#' their country before allowing them to access the app.
#'
#' @param d2_session DHIS2 session object
#'
#' @return TRUE or FALSE indicating whether user is authorized to use the app
#' @export
#'
credential_check <- function(d2_session) {
  user_groups <- d2_session$me$userGroups$id
  user_roles <- d2_session$me$userCredentials$user_roles$id
  # Checks to see if user has correct credentials to access app
  if ("gh9tn4QBbKZ" %in% user_groups # Checks if user is Global user
      || "OoiLAfMTyMx" %in% user_groups # Checks if user has MoH read access
      || "jtzbVV4ZmdP" %in% user_roles) { # Checks if user is superuser
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Dropdown Menu Options -----------------------------------------------------

#' Get User Operating Units
#'
#' @description Finds the list of operating units for which a user is authorized.
#' If a user is a global user, then gives them access to all DAA countries.
#'
#' @param d2_session DHIS2 session object
#'
#' @return List of organisation units for which the user is authorized
#' @export
#'
#' @examples
get_user_ous <- function(d2_session = d2_session) {
  daa_countries <- daa.analytics::daa_countries %>%
    dplyr::arrange(OU)
  user_orgunit <- d2_session$user_orgunit

  # If user does not have global access, only return their assigned country.
  if (!("ybg3MO3hcf4" %in% user_orgunit)) {
    daa_countries %<>%
      dplyr::filter(OU_UID %in% user_orgunit)
  }

  ou_list <- stats::setNames(daa_countries$OU_UID,
                             daa_countries$OU)

  return(ou_list)
}

#' Get Indicator List
#' @description Returns a list of the indicators
#' with both the name used in the dataframes and the
#' name used in the dropdown menu options.
#'
#' @return List of indicators with names set
#' @export
#'
get_indicator_list <- function() {
  df <- tibble::tribble(
    ~indicator_name, ~indicator_code,
    "HTS_TST", "HTS_TST",
    "PMTCT_ART", "PMTCT_ART",
    "PMTCT_STAT", "PMTCT_STAT",
    "TB_PREV (2020-Present)", "TB_PREV",
    "TB_PREV (2018-2019)", "TB_PREV_LEGACY",
    "TX_NEW", "TX_NEW",
    "TX_CURR", "TX_CURR",
    "TX_PVLS_NUM", "TX_PVLS_NUM",
    "TX_PVLS_DEN","TX_PVLS_DEN"
  )

  de_list <- stats::setNames(df$indicator_code, df$indicator_name)

  return(de_list)
}

#' Get Period List
#'
#' @description Returns a list of valid fiscal years for
#' use in dropdown menus
#'
#' @return A list of fiscal years with both names as strings and integers
#' @export
#'
get_period_list <- function() {
  df <- data.frame(
    period_code = c(2018:daa.analytics::current_fiscal_year()),
    period_name = c(paste0("FY", 2018:daa.analytics::current_fiscal_year()))
  )

  pe_list <- stats::setNames(df$period_code, df$period_name)

  return(pe_list)
}

# Workbook File Functions --------------------------------------------------

#' Adorn Export Data
#'
#' @description Adds a "facility_hierarchy" column to data being exported.
#'
#' @param d List object containing data
#'
#' @return A dataframe adorned with "facility_hierarchy" column
#' @export
#'
adorn_export_data <- function(d) {

  if (is.null(d) || is.null(d$combined_data)) {
    return("No data available for export.")
  }

  df <- d %>%
    purrr::pluck("combined_data") %>%
    dplyr::mutate(
      facility_hierarchy = ifelse(is.na(SNU3),
                                  paste(OU, SNU1, SNU2, Facility, sep = "/"),
                                  paste(OU, SNU1, SNU2, SNU3, Facility, sep = "/")),
      difference = pepfar - moh)

  return(df)
}

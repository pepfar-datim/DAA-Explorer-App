# Configuration ---------------------------------------------------------------

#Initiate logging
logger <- futile.logger::flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

futile.logger::flog.appender(futile.logger::appender.console(),
                             name = "daa-analysis")

# User Access Information ---------------------------------------------------

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

get_user_ous <- function(d2_session = d2_session) {
  daa_countries <- daa.analytics::daa_countries %>%
    dplyr::arrange(country_name)
  user_orgunit <- d2_session$user_orgunit

  # If user does not have global access, only return their assigned country.
  if (!("ybg3MO3hcf4" %in% user_orgunit)) {
    daa_countries %<>%
      dplyr::filter(country_uid %in% user_orgunit)
  }

  ou_list <- stats::setNames(daa_countries$country_uid,
                             daa_countries$country_name)

  return(ou_list)
}

get_indicator_list <- function() {
  df <- daa.analytics::daa_indicators %>%
    dplyr::mutate(indicator_name = case_when(
      indicator == "TB_PREV" ~ "TB_PREV (2020-Present)",
      indicator == "TB_PREV_LEGACY" ~"TB_PREV (2018-2019)",
      TRUE ~ indicator
    )) %>%
    dplyr::select(indicator_name, indicator_code = indicator) %>%
    unique()

  de_list <- stats::setNames(df$indicator_code, df$indicator_name)

  return(de_list)
}

get_period_list <- function() {
  df <- data.frame(
    period_code = c(2018:daa.analytics::current_fiscal_year()),
    period_name = c(paste0("FY", 2018:daa.analytics::current_fiscal_year()))
  )

  pe_list <- stats::setNames(df$period_code, df$period_name)

  return(pe_list)
}

# S3 Timestamps ------------------------------------------------------------
s3_timestamp <- function() {
  # TODO: Once the S3 bucket timestamps are able to be retrieved, replace this.
  t1 <- lubridate::now()
  t2 <- lubridate::as_datetime(
    paste(lubridate::today("UTC"), "08:00:00"), tz = "UTC")

  if (t1 - (t2 + lubridate::hours(1)) > 0) {
    # Sets S3 refresh time to 8am UTC today if at least one hour past that time
    s3_time <- t2
  } else {
    # Sets S3 refresh time to 8am UTC yesterday if not at least one hour past
    s3_time <- t2 - lubridate::days(1)
  }

  return(s3_time)
}

# Workbook File Functions --------------------------------------------------

adorn_export_data <- function(d, wb_raw_uids_check = NULL) {

  if (is.null(d) || is.null(d$combined_data)) {
    return("No data available for export.")
  }

  df <- d %>%
    purrr::pluck("combined_data") %>%
    dplyr::mutate(
      facility_hierarchy = ifelse(is.na(namelevel7),
                                  paste(namelevel3, namelevel4, namelevel5,
                                        namelevel6, sep = "/"),
                                  paste(namelevel3, namelevel4, namelevel5,
                                        namelevel6, namelevel7, sep = "/")),
      difference = pepfar - moh) %>%
    dplyr::select(dplyr::starts_with("namelevel"), facilityuid, indicator,
                  period, moh, pepfar, difference, dplyr::everything())

  if (is.null(wb_raw_uids_check) || !wb_raw_uids_check){
    return(df)
  } else {
    hierarchy_uids <- d$ou_metadata %>%
      tidyr::separate(col = path,
                      into = c(NA, NA, NA, paste0("level", 3:7, "_uid"))) %>%
      dplyr::select(uid, tidyselect::starts_with("level"))

    df %<>%
      dplyr::left_join(hierarchy_uids, by = c("facilityuid" = "uid"))
  }
  return(df)
}

wb_filename <- function(d, type) {

  if (is.null(d) || is.null(d$combined_data)) {
    return("no_data.txt")
  }

  date <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
  ou_name <- d$ou_name

  if (type == "raw") {
    name <- paste0(paste(date, ou_name, "raw_data", sep = "_"), ".csv")
  } else {
    name <-
      paste0(paste(date, ou_name, "analysis_workbook", sep = "_"), ".xlsx")
  }

  return(name)
}

wb_filecontent <- function(df, file) {

  # Opens template to use for analysis workbooks
  wb <- openxlsx::loadWorkbook(file = file.path("templates", "template.xlsx"))

  openxlsx::removeTable(wb = wb, sheet = "RawData", table = "rawdata")

  openxlsx::writeDataTable(
    wb = wb,
    sheet = "RawData",
    x = df,
    xy = c(1, 1),
    colNames = T,
    rowNames = F,
    tableStyle = "TableStyleMedium16",
    tableName = "rawdata",
    withFilter = TRUE,
    bandedRows = TRUE
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  return(wb)
}

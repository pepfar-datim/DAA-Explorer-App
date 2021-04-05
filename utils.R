
# CONFIGURATION ---------------------------------------------------------------

daa_countries <- read.csv("daa_countries.csv")

#Initiate logging
logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name="daa-analysis")


# LOGIN FUNCTIONS -------------------------------------------------------------

dhis_login <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(baseurl, "api/me"))
  #Logging in here will give us a handle which we need to return if successful

  httr_handle <- httr::handle(URLencode(baseurl))

  r <- httr::with_config(config =
                           httr::config(httpauth = 1,
                                        userpwd =
                                          paste0(username, ":", password)),
                         httr::GET(url = url, handle = httr_handle),
                         override = TRUE)

  if (r$status_code != 200L) {
    result <- list(logged_in = FALSE)
    return(result)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))

    if (baseurl == Sys.getenv("D4U_URL")) {
      organisation_unit <- "FETQ6OmnsKB"
    } else{
      organisation_unit <- me$organisationUnits$id
    }

    # If user does not have global access, indicate that they are country user.
    if (organisation_unit != "ybg3MO3hcf4") {
      country_level_user <- TRUE
    } else {
      country_level_user <- FALSE
    }

    user_groups <- me$userGroups$id
    user_roles <- me$userCredentials$userRoles$id

    result <- list(logged_in = TRUE,
                  handle = httr_handle,
                  org_unit = organisation_unit,
                  country_level_user = country_level_user,
                  user_groups = user_groups,
                  user_roles = user_roles)
    return(result)
  }
}

credential_check <- function(user_groups, user_roles) {

  # Checks to see if user has correct credentials to access app
  if ("gh9tn4QBbKZ" %in% user_groups # Checks if user is Global user
      || "OoiLAfMTyMx" %in% user_groups # Checks if user has MoH read access
      || "jtzbVV4ZmdP" %in% user_roles) { # Checks if user is superuser
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Loads credentials based on configuration file path
load_config_file <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at", config_path))
    }
    dhis_config <- jsonlite::fromJSON(config_path)
    return(dhis_config)
  } else {
    stop("You must specify a credentials file!")
  }
}


# DHIS2 API CALL HANDLER FUNCTIONS --------------------------------------------

# Function for calling the analytics endpoint of DATIM
d2_analyticsresponse <- function(url, remap_cols = TRUE, httr_handle) {
  d <- jsonlite::fromJSON(httr::content(httr::GET(url = url,
                                                  handle = httr_handle),
                                        as = "text"))
  if (NROW(d$rows) > 0) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               function(x) {
                                 data.frame(x, stringsAsFactors = FALSE) %>%
                                   dplyr::select(name)
                               })) %>%
      dplyr::mutate(., from = row.names(.))
    remap_meta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }

    d <- tibble::as_tibble(d$rows,
                           .name_repair = "minimal") %>%
      `names<-`(., d$headers$column)
    if (remap_cols == TRUE) {
      d <- plyr::colwise(remap_meta)(d)
    }
    return(d)
  } else {
    return(NULL)
  }
}

# Simpler function for handling basic calls to DHIS2
d2_basicresponse <- function(url, httr_handle) {

  # Fetches data from the server
  df <- jsonlite::fromJSON(httr::content(httr::GET(url = url,
                                                   handle = httr_handle),
                                         as = "text"))

  if (is.null(df)) {
    return(NULL)
  } else {
    return(df)
  }

}

# Function for handling downloading CSV files from DHIS2
d2_csvresponse <- function(url, httr_handle) {

  # Fetches data from the server
  x <- httr::content(httr::GET(url = url, handle = httr_handle), as = "text")

  df <- read.csv(text = x)

  return(df)

}

# GET OU INFORMATION FUNCTIONs ------------------------------------------------

get_ou_name <- function(ou_uid, d4u_user = FALSE) {

  ou_name <- daa_countries$countryName[daa_countries$countryUID == ou_uid] %>%
    toString(.)

  # Returns OU name
  return(ou_name)
}

get_org_unit_info <- function(ou_name, d4u_user = FALSE, handle) {

  # Assembles URL for API call
  if (d4u_user == FALSE) {
    base_url <- Sys.getenv("DATIM_URL")
  } else {
    base_url <- Sys.getenv("D4U_URL")
  }

  url <- glue::glue("{base_url}api/dataStore/
                        dataSetAssignments/orgUnitLevels") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)


  # Fetches data from the server
  df <- d2_basicresponse(url, handle)

  if (is.null(df)) {
    return(NULL)
  }

  df <- do.call(rbind,
                lapply(df,
                       function(x) {
                         data.frame(x, stringsAsFactors = FALSE)
                       }))

  # Returns list with country level, facility level, and abbreviation
  org_unit_info <- list(
    country_level = df[ou_name, "country"],
    facility_level = df[ou_name, "facility"],
    abbreviation = df[ou_name, "iso3"]
  )

  return(org_unit_info)
}

get_user_ous <- function(user_orgunit) {

  if (is.null(user_orgunit)) {
    return("")
  }

  # If user does not have global access, only return their assigned country.
  if (user_orgunit != "ybg3MO3hcf4") {
    daa_countries %<>%
      dplyr::filter(countryUID == user_orgunit)
  }

  ou_list <- stats::setNames(daa_countries$countryUID,
                             daa_countries$countryName)

  return(ou_list)
}

# API CALL FUNCTIONS ---------------------------------------------------------

get_indicators_table <- function(ou_uid,
                                 d4u_user = FALSE,
                                 facility_level,
                                 country_level_user,
                                 # period_list, # TODO add back in when fixed
                                 httr_handle) {

  # Assembles URL for API call based on the facility level of the OU

  if (d4u_user == FALSE) {
    base_url <- Sys.getenv("DATIM_URL")
  } else {
    base_url <- Sys.getenv("D4U_URL")
  }

  period_list <- "2017Oct;2018Oct;2019Oct"

  lvl_url <- ifelse(d4u_user == FALSE,
                    ifelse(facility_level == 7,
                           "LEVEL-7;",
                           "LEVEL-6;"),
                    "LEVEL-7;")

  url <- glue::glue(paste0("{base_url}api/analytics.csv?",
                           "dimension=SH885jaRe0o:mXjFJEexCHJ;t6dWOH7W5Ml&",
                           "dimension=ou:{lvl_url}{ou_uid}&",
                           "dimension=dx:",
                           "xNTzinnVgba;",             # HTS_TST 2018-2019
                           "V6hxDYUZFBq;BRalYZhcHpi;", # HTS_TST 2020 (filter)
                           "yEQ5FoXJWAx;",             # PMTCT_ART 2018-2020
                           "aeCf1jJWE1x;",             # PMTCT_STAT 2018-2019
                           "qFWmLNueTPF;iDf461nJDJr;", # PMTCT_STAT 2020
                           "sdarqD1J8fb;GxUQu72i38n;", # TX_CURR 2018-2020
                           "Mon8vQgC9qg;l697bKzFRSv;", # TX_NEW 2018-2020
                           "J1E7eh1CyA0;LZbeWYZEkYL;", # TB_PREV 2018-2019
                           "oFFlA4vaSWD&",             # TB_PREV 2020
                           "dimension=pe:{period_list}&",
                           "displayProperty=SHORTNAME&",
                           "tableLayout=true&",
                           "columns=SH885jaRe0o&",
                           "rows=ou;dx;pe&",
                           "showHierarchy=true&",
                           "skipData=false&",
                           "includeMetadataDetails=false"
  )) %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- d2_csvresponse(url, httr_handle)

  # Returns null if API returns nothing
  if (is.null(df)) {
    return(NULL)
  }

  # Renames organization unit level columns for country users
  if (country_level_user == TRUE && facility_level == 7) {
    df %<>%
      dplyr::select("orgunitlevel3" = orgunitlevel1,
                    "orgunitlevel4" = orgunitlevel2,
                    "orgunitlevel5" = orgunitlevel3,
                    "orgunitlevel6" = orgunitlevel4,
                    "orgunitlevel7" = orgunitlevel5,
                    organisationunitid,
                    dataname,
                    periodname,
                    `X00100...PEPFAR.MOH.align..MOH.Data`,
                    `X00200...PEPFAR.MOH.align..PEPFAR.Data`
      )
  } else if (country_level_user == TRUE && facility_level == 6) {
    df %<>%
      dplyr::select("orgunitlevel3" = orgunitlevel1,
                    "orgunitlevel4" = orgunitlevel2,
                    "orgunitlevel5" = orgunitlevel3,
                    "orgunitlevel6" = orgunitlevel4,
                    organisationunitid,
                    dataname,
                    periodname,
                    `X00100...PEPFAR.MOH.align..MOH.Data`,
                    `X00200...PEPFAR.MOH.align..PEPFAR.Data`
      )
  }

  # Renames organization unit level columns and adds Site Hierarchy column
  if (facility_level == 7) {

    df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "namelevel7" = orgunitlevel7,
                    "organisationunitid" = organisationunitid,
                    "indicator" = dataname,
                    "period" = periodname,
                    "MOH" = `X00100...PEPFAR.MOH.align..MOH.Data`,
                    "PEPFAR" = `X00200...PEPFAR.MOH.align..PEPFAR.Data`) %>%
      dplyr::filter(!is.na(namelevel7) & (namelevel7 != "")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4,
                                             namelevel5, namelevel6,
                                             namelevel7, sep = " / "))

  } else{

    df %<>%
      dplyr::select("namelevel3" = orgunitlevel3,
                    "namelevel4" = orgunitlevel4,
                    "namelevel5" = orgunitlevel5,
                    "namelevel6" = orgunitlevel6,
                    "organisationunitid" = organisationunitid,
                    "indicator" = dataname,
                    "period" = periodname,
                    "MOH" = `X00100...PEPFAR.MOH.align..MOH.Data`,
                    "PEPFAR" = `X00200...PEPFAR.MOH.align..PEPFAR.Data`) %>%
      dplyr::filter(!is.na(namelevel6) & (namelevel6 != "")) %>%
      dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4, namelevel5,
                                             namelevel6, sep = " / "))

  }

  # Cleans up indicator, MOH, and PEPFAR columns
  df %<>%
    # Cleans Period data from the form `Oct 2017 to Sept 2018` to `2018`
    dplyr::mutate(period = as.numeric(stringr::str_sub(period, -4, -1))) %>%

    # Filtering out HTS_TST data from indicator V6hxDYUZFBq to only FY20
    dplyr::filter((indicator == "HTS_TST (N, MOH, Age/Sex/Result)"
                    & period == 2020) |
                    (indicator != "HTS_TST (N, MOH, Age/Sex/Result)")) %>%

    # Filtering out HTS_TST data from indicator BRalYZhcHpi to only FY20
    dplyr::filter((indicator == "HTS_TST (N, MOH, Age Agg/Sex/Result)"
                     & period == 2020) |
                  (indicator != "HTS_TST (N, MOH, Age Agg/Sex/Result)")) %>%

    # TODO separate and display both TB_PREV disaggregation types side-by-side
    # Filters out TB_PREV Therapy Type indicator data so there is no duplication
    dplyr::filter(indicator != "TB_PREV (N, MOH, TherapyType/NewExArt/HIV)") %>%

    # Renames TB_PREV indicators for legacy and new before parsing
    dplyr::mutate(indicator = as.character(indicator)) %>%
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "TB_PREV (N, MOH, Age Agg/Sex/HIVStatus)" ~ "TB_PREV_LEGACY",
      indicator == "TB_PREV (N, MOH, Age, Sec, ART Start)" ~ "TB_PREV",
      TRUE ~ indicator
    )) %>%

    # Cleans indicator names to just the short name
    tidyr::separate(indicator, c("indicator"), sep = " ", extra = "drop") %>%

    # Renames TB_PREV indicators to indicate valid years
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "TB_PREV_LEGACY" ~ "TB_PREV (FY2018-FY2019)",
      indicator == "TB_PREV" ~ "TB_PREV (FY2020-Present)",
      TRUE ~ indicator
    )) %>%

    # Converts MOH and PEPFAR columns to numeric data types
    dplyr::mutate("MOH" = as.numeric(MOH)) %>%
    dplyr::mutate("PEPFAR" = as.numeric(PEPFAR))

  # Summarizes MOH and PEPFAR data up from coarse and fine disaggregates
  if (facility_level == 7) {

    df %<>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                      namelevel7, organisationunitid, indicator,
                      period, `Site hierarchy`) %>%
      dplyr::summarise(MOH = sum(MOH, na.rm = any(!is.na(MOH))),
                       PEPFAR = sum(PEPFAR, na.rm = any(!is.na(PEPFAR)))) %>%
      dplyr::ungroup()

  } else {

    df %<>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5, namelevel6,
                      organisationunitid, indicator,
                      period, `Site hierarchy`) %>%
      dplyr::summarise(MOH = sum(MOH, na.rm = any(!is.na(MOH))),
                       PEPFAR = sum(PEPFAR, na.rm = any(!is.na(PEPFAR)))) %>%
      dplyr::ungroup()

  }

  # Creates basic summary columns about reporting institutions and figures
  df %<>%
    dplyr::mutate("Reported on by both" =
                    ifelse(is.na(MOH) | is.na(PEPFAR),
                           ifelse(is.na(MOH) & is.na(PEPFAR), "neither", "one"),
                           "both")) %>%
    dplyr::mutate("Reported by" =
                    ifelse(!is.na(MOH),
                           ifelse(!is.na(PEPFAR), "Both", "MOH"),
                           ifelse(!is.na(PEPFAR), "PEPFAR", "Neither"))) %>%
    dplyr::mutate("Difference" =
                    ifelse(`Reported by` == "Both", MOH - PEPFAR, NA)) %>%
    dplyr::mutate("Reported higher" = dplyr::case_when(
      is.na(MOH) ~ "Only PEPFAR reported",
      is.na(PEPFAR) ~ "Only MOH reported",
      Difference > 0 ~ "MOH reported higher",
      Difference < 0 ~ "PEPFAR reported higher",
      Difference == 0 ~ "Same result reported",
      TRUE ~ "Neither reported"
    ))

  # Groups rows by indicator and calculates indicator-specific summaries
  df %<>%
    dplyr::group_by(indicator, period) %>%
    dplyr::mutate("Count of sites reporting both" =
                    sum(ifelse(`Reported on by both` == "both", 1, 0))) %>%
    dplyr::mutate("PEPFAR sum of sites reporting both" =
                    sum(ifelse(`Reported on by both` == "both", PEPFAR, 0))) %>%
    dplyr::ungroup()

  # Calculates weighting variables
  df %<>%
    dplyr::mutate("Weighting" =
                    ifelse(`Reported by` == "Both",
                           PEPFAR / `PEPFAR sum of sites reporting both`,
                           NA)) %>%
    dplyr::mutate("Average" = rowMeans(cbind(MOH, PEPFAR), na.rm = F)) %>%
    dplyr::mutate("Weighted difference" =
                    ifelse(`Reported by` == "Both",
                           Weighting * abs(Difference) / Average, NA))

  # Reorganizes table for export
  df %<>%
    dplyr::select(
      dplyr::starts_with("namelevel"),
      organisationunitid, indicator, period, MOH, PEPFAR,
      `Reported on by both`, `Reported by`, `Reported higher`,
      Difference, Weighting, `Weighted difference`,
      `Count of sites reporting both`,
      `PEPFAR sum of sites reporting both`,
      `Site hierarchy`
      )

  return(df)
}

fetch_s3_files <- function(s3, Bucket, Key_1, Key_2) {

  s3_object <-
    s3$get_object(Bucket = Bucket,
                  Key = paste0(Key_1, Key_2, "/data.csv.gz"))
  s3_object_body <- s3_object$Body

  file_name2 <- paste0("s3_files/", Key_2, ".csv.gz")
  if (file.exists(file_name2)) {
    unlink(file_name2)
  }

  writeBin(s3_object_body, con = file_name2)
  data <- data.table::fread(file_name2)
  flog.info(paste0("Retreived S3 file to ", file_name2))
  if(!file.exists(file_name2)) {stop("Could not retreive support file.")}

  return(data)
}

get_pvls_emr_table <- function() {

  s3 <- paws::s3()
  pvls_emr <- 
    fetch_s3_files(
      s3 = s3,
      Bucket = Sys.getenv("AWS_S3_BUCKET"),
      Key_1 = "datim/www.datim.org/",
      Key_2 = "moh_daa_data_value_emr_pvls"
    )

  return(pvls_emr)
}

pvls_emr_metadata <- function(ou_name, pvls_emr, country_level) {

  s3 <- paws::s3()
  s3_bucket <- Sys.getenv("AWS_S3_BUCKET")

  de_metadata <- 
    fetch_s3_files(
      s3 = s3,
      Bucket = Sys.getenv("AWS_S3_BUCKET"),
      Key_1 = "datim/www.datim.org/",
      Key_2 = "data_element"
    ) %>%
    dplyr::select(dataelementid, "dataelementname" = shortname)
  
  coc_metadata <-
    fetch_s3_files(
      s3 = s3,
      Bucket = Sys.getenv("AWS_S3_BUCKET"),
      Key_1 = "datim/www.datim.org/",
      Key_2 = "category_option_combo"
    ) %>%
    dplyr::select(categoryoptioncomboid, categoryoptioncomboname = name)
  
  ou_metadata <- 
    fetch_s3_files(
      s3 = s3,
      Bucket = Sys.getenv("AWS_S3_BUCKET"),
      Key_1 = "datim/www.datim.org/",
      Key_2 = "organisation_unit"
    ) %>%
    dplyr::select(organisationunitid, path, name = shortname, uid)

  ou_uid_names <- ou_metadata %>% dplyr::select(uid, name)

  # Cleans and creates OU Hierarchy from levels 3 to 7 with names
  ou_hierarchy <- ou_metadata %>%
    tidyr::separate(.,
                    col = path,
                    into = paste0("namelevel", 0:9, "uid"),
                    sep = "/") %>%
    dplyr::left_join(.,
                     ou_uid_names %>%
                       dplyr::select(uid, namelevel3 = name),
                     by = c("namelevel3uid" = "uid")) %>%
    dplyr::left_join(.,
                     ou_uid_names %>%
                       dplyr::select(uid, namelevel4 = name),
                     by = c("namelevel4uid" = "uid")) %>%
    dplyr::left_join(.,
                     ou_uid_names %>%
                       dplyr::select(uid, namelevel5 = name),
                     by = c("namelevel5uid" = "uid")) %>%
    dplyr::left_join(.,
                     ou_uid_names %>%
                       dplyr::select(uid, namelevel6 = name),
                     by = c("namelevel6uid" = "uid")) %>%
    dplyr::left_join(.,
                     ou_uid_names %>%
                       dplyr::select(uid, namelevel7 = name),
                     by = c("namelevel7uid" = "uid")) %>%
    dplyr::select(organisationunitid,
                  namelevel6uid, namelevel7uid,
                  paste0("namelevel", 3:7)) %>%
    dplyr::mutate("Site hierarchy" = paste(namelevel3, namelevel4, namelevel5,
                                           namelevel6, sep = " / "))

  # Filters facilities to country
  if (country_level == 3) {
    ou_hierarchy %<>%
      dplyr::filter(namelevel3 == ou_name)
  } else if (country_level == 4) {
    ou_hierarchy %<>%
      dplyr::filter(namelevel4 == ou_name)
  }

  daa_pe_metadata <-
    fetch_s3_files(
      s3 = s3,
      Bucket = Sys.getenv("AWS_S3_BUCKET"),
      Key_1 = "datim/www.datim.org/",
      Key_2 = "moh_daa_period_structure"
    ) %>%
    dplyr::select(periodid, iso)

  pvls_emr %<>%
    # Joins to period data and cleans and filters periods
    dplyr::left_join(., daa_pe_metadata, by = "periodid") %>%

    # Filters for only Calendar Q3 / Fiscal Q4 results
    dplyr::filter(base::substring(iso, 5, 6) == "Q3") %>%
    dplyr::mutate(period = as.numeric(base::substring(iso, 1, 4))) %>%

    # Joins to Data Element, Category Option Combo, and Attribute Metadata
    dplyr::left_join(., de_metadata, by = "dataelementid") %>%
    dplyr::left_join(., coc_metadata, by = "categoryoptioncomboid") %>%
    # dplyr::left_join(.,
    #                  coc_metadata %>%
    #                    dplyr::select(categoryoptioncomboid,
    #                                  attributename = coc_name),
    #                  by = c("attributeoptioncomboid" =
    #                           "categoryoptioncomboid")) %>%

    # Drops a number of columns before continuing on
    dplyr::select(-dataelementid, -periodid,
                  -categoryoptioncomboid, -attributeoptioncomboid, -iso) %>%

    # Cleans indicator names and pivots data
    dplyr::mutate(indicator = dplyr::case_when(
      dataelementname == "EMR_SITE (N, NoApp, Serv Del Area)" &
        categoryoptioncomboname ==
        "Service Delivery Area - Care and Treatment" ~
        "EMR - Care and Treatment",
      dataelementname == "EMR_SITE (N, NoApp, Serv Del Area)" &
        categoryoptioncomboname ==
        "Service Delivery Area - HIV Testing Services" ~
        "EMR - HIV Testing Services",
      dataelementname == "EMR_SITE (N, NoApp, Serv Del Area)" &
        categoryoptioncomboname ==
        "Service Delivery Area - ANC and/or Maternity" ~
        "EMR - ANC and/or Maternity",
      dataelementname == "EMR_SITE (N, NoApp, Serv Del Area)" &
        categoryoptioncomboname ==
        "Service Delivery Area - Early Infant Diagnosis (not Ped ART)" ~
        "EMR - EID",
      dataelementname == "EMR_SITE (N, NoApp, Serv Del Area)" &
        categoryoptioncomboname == "Service Delivery Area - HIV/TB" ~
        "EMR - HIV/TB",
      base::substring(dataelementname, 1, 10) == "TX_PVLS (N" ~ "TX_PVLS_N",
      base::substring(dataelementname, 1, 10) == "TX_PVLS (D" ~ "TX_PVLS_D",
      TRUE ~ NA_character_
    )) %>%

    # TODO Clean and bring categoryOptionCombos into the rest of the app
    dplyr::select(-dataelementname, -categoryoptioncomboname) %>%
    tidyr::pivot_wider(.,
                       names_from = indicator,
                       values_from = value,
                       values_fn = list) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `EMR - Care and Treatment` =
        any(as.logical(unlist(`EMR - Care and Treatment`))),
      `EMR - HIV Testing Services` =
        any(as.logical(unlist(`EMR - HIV Testing Services`))),
      `EMR - ANC and/or Maternity` =
        any(as.logical(unlist(`EMR - ANC and/or Maternity`))),
      `EMR - EID` =
        any(as.logical(unlist(`EMR - EID`))),
      `EMR - HIV/TB` =
        any(as.logical(unlist(`EMR - HIV/TB`))),
      TX_PVLS_N = sum(as.numeric(unlist(TX_PVLS_N))),
      TX_PVLS_D = sum(as.numeric(unlist(TX_PVLS_D)))
    ) %>%

    # Joins to Organizational hierarchy data
    dplyr::left_join(., ou_hierarchy,
                     by = c("sourceid" = "organisationunitid")) %>%

    # Organizes columns for export
    dplyr::select(starts_with("namelevel"),
                  `Site hierarchy`,
                  period,
                  `EMR - HIV Testing Services`,
                  `EMR - Care and Treatment`,
                  `EMR - ANC and/or Maternity`,
                  `EMR - EID`,
                  `EMR - HIV/TB`,
                  TX_PVLS_N,
                  TX_PVLS_D,
                  -sourceid
    )

  return(pvls_emr)
}

get_geoalign_table <- function(geo_handle) {

  # Assembles URL for API call
  base_url <- Sys.getenv("GEOALIGN_URL")

  url <- glue::glue("{base_url}api/dataStore/MOH_country_indicators/") %>%
    URLencode(.)

  # Fetches data from the server
  df <- d2_basicresponse(url, geo_handle)

  if (is.null(df)) {
    return(NULL)
  }

  df %<>%
    lapply(.,
           function(x) {
             paste0(url,
                    x) %>%
               d2_basicresponse(., geo_handle) %>%
               as.data.frame(.) %>%
               mutate(period = x)
           }) %>%
    do.call(plyr::rbind.fill, .) %>%
    tidyr::pivot_longer(-c(period, CountryName, CountryCode, generated),
                        names_sep = "_(?=[^_]*$)",
                        names_to = c("indicator", ".value")) %>%
    dplyr::mutate(period = as.numeric(period),
                  hasDisagMapping = ifelse(hasDisagMapping %in%
                                             c("No", "NA", NA),
                                           "No",
                                           hasDisagMapping)) %>%
    dplyr::mutate(hasResultsData =
                    ifelse(period == max(period),
                           hasResultsData,
                           NA_character_)) %>%
    dplyr::select(CountryName, period, indicator,
                  hasDisagMapping, hasResultsData)

  return(df)

  # TODO fix this to pull in list of periods dynamically for use elsewhere
  # Pulls all years from GeoAlign data and adds 2018
  # valid_years <- df$period %>%
  #   unique(.) %>%
  #   as.numeric(.) %>%
  #   append(., 2018) %>%
  #   sort(., decreasing = TRUE) %>%
  #   data.frame(periodValue = .) %>%
  #   dplyr::mutate(periodName = paste0("FY", as.character(periodValue)),
  #                 datimFilter = paste0(as.character(periodValue - 1), "Oct"))
  #
  # period_list <- valid_years$datimFilter %>%
  #   sort(.) %>%
  #   paste(., collapse = ";")
  #
  # valid_years <-
  #   stats::setNames(valid_years$periodValue, valid_years$periodName)
  #
  # geo <- list(geo_data = df,
  #             valid_years = valid_years,
  #             period_list = period_list)
  #
  # return(geo)

}

get_attribute_table <- function(ou_uid, d4u_user = FALSE, httr_handle) {

  if (d4u_user == FALSE) {
    base_url <- Sys.getenv("DATIM_URL")
  } else {
    base_url <- Sys.getenv("D4U_URL")
  }

  url <- glue::glue("{base_url}api/organisationUnits.json?
                filter=path:like:{ou_uid}&
                fields=id,name,geometry,
                attributeValues[attribute[id,name],value]&
                paging=false") %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    URLencode(.)

  # Fetches data from the server
  df <- d2_basicresponse(url = url, httr_handle = httr_handle)

  # Returns null if API returns nothing
  if (is.null(df)) {
    return(NULL)
  }

  df %<>%
    purrr::pluck(., "organisationUnits") %>%
    data.frame(., stringsAsFactors = FALSE) %>%
    # Unnests and pivots the data from the site attributes column
    tidyr::unnest(., cols = "attributeValues") %>%
    dplyr::mutate(`MOH ID` = ifelse(attribute[, 1] == "MOH ID",
                                    value,
                                    NA)) %>%
    dplyr::mutate(`coordinates` =
                    ifelse(geometry[, 1] == "Point",
                           as.character(geometry[, 2]) %>%
                             stringr::str_extract("(?<=\\()(.*?)(?=\\))"),
                           NA)) %>%
    dplyr::filter(!is.na(`MOH ID`)) %>%

    # Cleans the geometry data
    tidyr::separate(.,
                    col = `coordinates`,
                    into = c("longitude", "latitude"),
                    sep = ",") %>%
    dplyr::mutate(across(.cols = c("longitude", "latitude"), as.numeric)) %>%

    # Selects only the correct columns to be used
    dplyr::select(name, id, `MOH ID`,
                  longitude, latitude)

  return(df)

}

combine_data <- function(indicators, pvls_emr, site_attr, facility_level) {

  if (facility_level == 6) {
    df <- indicators %>%
      dplyr::left_join(pvls_emr %>%
                         select(-namelevel3, -namelevel4, -namelevel5,
                                -namelevel6, -namelevel7, -`Site hierarchy`),
                       by = c("organisationunitid" = "namelevel6uid",
                              "period" = "period")) %>%
      dplyr::left_join(site_attr, by = c("organisationunitid" = "id")) %>%
      dplyr::select(dplyr::starts_with("namelevel"), dplyr::everything(),
                    -namelevel7uid, -name)
  } else {
    df <- indicators %>%
      dplyr::left_join(pvls_emr %>%
                         select(-namelevel3, -namelevel4, -namelevel5,
                                -namelevel6, -namelevel7, -`Site hierarchy`),
                       by = c("organisationunitid" = "namelevel7uid",
                              "period" = "period")) %>%
      dplyr::left_join(site_attr, by = c("organisationunitid" = "id")) %>%
      dplyr::select(dplyr::starts_with("namelevel"), dplyr::everything(),
                    -namelevel6uid, -name)
  }

  return(df)

}

# WORKBOOK FILE FUNCTIONS --------------------------------------------------

wb_filename <- function(ou, my_indicator = NA, suffix, file_type = ".xlsx") {

  date <- base::format(Sys.time(), "%Y%m%d_%H%M%S")

  if (!is.na(my_indicator)) {

    name <- paste0(paste(ou, my_indicator, suffix, date, sep = "_"), file_type)

  } else {

    name <- paste0(paste(ou, suffix, date, sep = "_"), file_type)

  }

  return(name)

}

wb_filecontent <- function(d, my_indicator, file) {

  seventh_col <- c(namelevel7 = NA_character_)

  analytics <- d %>%
    purrr::pluck(., "analytics") %>%
    dplyr::filter(indicator == my_indicator) %>%
    tibble::add_column(.,
                       !!!seventh_col[!base::names(seventh_col) %in%
                                        base::names(.)]) %>%
    dplyr::select(dplyr::starts_with("namelevel"), dplyr::everything())

  # Selects correct template to use for this workbook
  if (my_indicator == "TX_CURR" | my_indicator == "TX_NEW") {
    wb <- openxlsx::loadWorkbook(file = file.path("templates",
                                                  "template_TX.xlsx"))
  } else if (my_indicator == "HTS_TST") {
    wb <- openxlsx::loadWorkbook(file = file.path("templates",
                                                  "template_HTS.xlsx"))
  } else if (my_indicator == "PMTCT_STAT" | my_indicator == "PMTCT_ART") {
    wb <- openxlsx::loadWorkbook(file = file.path("templates",
                                                  "template_PMTCT.xlsx"))
  } else  if (my_indicator == "TB_PREV") {
    wb <- openxlsx::loadWorkbook(file = file.path("templates",
                                                  "template_TB.xlsx"))
  }

  openxlsx::removeTable(wb = wb, sheet = "RawData", table = "RawData")

  openxlsx::writeDataTable(
    wb = wb,
    sheet = "RawData",
    x = analytics,
    xy = c(1, 1),
    colNames = T,
    rowNames = F,
    tableStyle = "TableStyleMedium16",
    tableName = "RawData",
    withFilter = TRUE,
    bandedRows = TRUE
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  return(wb)

}

raw_filecontent <- function(d, file) {

  analytics <- d %>%
    purrr::pluck("analytics")

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "RawData")

  openxlsx::writeDataTable(wb = wb,
                           sheet = "RawData",
                           x = analytics,
                           xy = c(1, 1),
                           colNames = T,
                           rowNames = F,
                           tableStyle = "TableStyleMedium16",
                           tableName = "RawData",
                           withFilter = TRUE,
                           bandedRows = TRUE
  )

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  return(wb)

}

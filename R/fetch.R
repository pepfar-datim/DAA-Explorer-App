# Fetch Function ---------------------------------------------------------
# This is the function that fetches data from DATIM and GeoAlign when the "Get Data" button is pressed

fetch <- function(authenticated, ready, ou_uid, d2_session, geo_session) {
  shinyjs::disable("reset_input")
  shinyjs::hide("hr-toggle")
  shinyjs::hide("vz_de_input")
  shinyjs::hide("vz_pe_input")
  shinyjs::disable("save_con_graph")
  shinyjs::disable("integrity_radio_input")
  shinyjs::disable("save_pivot")
  shinyjs::disable("download_raw")

  if (!ready$ok) {
    return(NULL)
  } else {

    withProgress(message = "Fetching data", value = 0, {

      shinyjs::disable("ou")
      shinyjs::disable("fetch")
      shinyjs::enable("reset_input")

      # Creates list object with session objects included
      d <- list(d2_session = d2_session, geo_session = geo_session)
      aws_s3_bucket = Sys.getenv("AWS_S3_BUCKET")
      # TODO recalculate progress bars
      shiny::incProgress(0.0625, detail = ("Fetching OU information"))
      d$ou_uid <- ou_uid
      d$ou_name <- datimutils::getOrgUnits(d$ou_uid, d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching DAA Indicator data"))
      d$daa_indicator_data <-
        daa.analytics::get_daa_data(ou_uid = d$ou_uid,
                                    fiscal_year = c(2018:2022),
                                    d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Adorning DAA Indicator data"))
      print(d$d2_session)
      d$daa_indicator_data <- d$daa_indicator_data |> daa.analytics::adorn_daa_data(include_coc = FALSE,
                                                              d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Fetching category option combo metadata"))
      d$coc_metadata <-
        daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                   dataset_name = "coc_metadata",
                                   cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Fetching data element metadata"))
      d$de_metadata <-
        daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                   dataset_name = "de_metadata",
                                   cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Generating organisation unit hierarchy"))
      d$ou_hierarchy <-
        daa.analytics::create_hierarchy(aws_s3_bucket = aws_s3_bucket,
                                        cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching period metadata"))
      d$pe_metadata <-
        daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                   dataset_name = "pe_metadata",
                                   cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching PVLS and EMR data"))
      d$pvls_emr_raw <-
        daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                   dataset_name = "pvls_emr_raw",
                                   cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Adorning PVLS and EMR data"))
      d$pvls_emr <- daa.analytics::adorn_pvls_emr(pvls_emr_raw = d$pvls_emr_raw,
                                     coc_metadata = d$coc_metadata,
                                     de_metadata = d$de_metadata,
                                     pe_metadata = d$pe_metadata,
                                     aws_s3_bucket = aws_s3_bucket,
                                     cache_folder = "support_files"
                                     )
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching site attribute data"))
      d$attribute_data <-
        daa.analytics::get_attribute_table(ou_uid = d$ou_uid,
                                           d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Processing Adorn Weights"))
        d$daa_indicator_data <-
        daa.analytics::adorn_weights(daa_indicator_data = d$daa_indicator_data,
                                     weights_list = c("OU", "SNU1", "SNU2", "EMR"),
                                     ou_hierarchy = d$ou_hierarchy, pvls_emr = d$pvls_emr)
        Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching GeoAlign data"))
      d$import_history <-
        daa.analytics::get_import_history(geo_session = d$geo_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Checking data integrity: nulls"))
      d$integrity_nulls <- daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                                      dataset_name = "null_ids",
                                                      cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Checking data integrity: duplicates"))
      d$integrity_duplicates <- daa.analytics::get_s3_data(aws_s3_bucket = aws_s3_bucket,
                                                           dataset_name = "duplicate_ids",
                                                           cache_folder = "support_files")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Merging datasets"))

      d$combined_data <- daa.analytics::combine_data(
        daa_indicator_data = d$daa_indicator_data,
        ou_hierarchy = d$ou_hierarchy,
        pvls_emr = d$pvls_emr,
        attribute_data = d$attribute_data,
        cache_folder = "support_files")
      Sys.sleep(0.5)
    })

    if (is.null(d$pvls_emr) & is.null(d$daa_indicator_data)) {
      sendSweetAlert(
        session,
        title = "Oops!",
        text = "Sorry, I could not find any data for you!"
      )
      shinyjs::hide("hr-toggle")
      shinyjs::hide("vz_de_input")
      shinyjs::hide("vz_pe_input")
      shinyjs::disable("save_con_graph")
      shinyjs::disable("integrity_radio_input")
      shinyjs::disable("save_pivot")
      shinyjs::disable("download_raw")
      return(NULL)
    } else {
      shinyjs::show("hr-toggle")
      shinyjs::show("vz_de_input")
      shinyjs::show("vz_pe_input")
      shinyjs::enable("save_con_graph")
      shinyjs::enable("integrity_radio_input")
      shinyjs::enable("save_pivot")
      shinyjs::enable("download_raw")
      return(d)
    }
  }
}

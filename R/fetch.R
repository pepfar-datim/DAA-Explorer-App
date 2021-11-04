# Fetch Function ---------------------------------------------------------

fetch <- function(authenticated, ready, ou_uid, d2_session, geo_session) {
  shinyjs::disable("reset_input")
  shinyjs::hide("hr-toggle")
  shinyjs::hide("vz_de_input")
  shinyjs::hide("vz_pe_input")
  # shinyjs::hide("update_note")
  shinyjs::disable("save_con_graph")
  shinyjs::disable("integrity_radio_input")
  shinyjs::disable("save_pivot")
  shinyjs::disable("download_workbook")
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

      # TODO recalculate progress bars
      shiny::incProgress(0.0625, detail = ("Fetching OU information"))
      d$ou_uid <- ou_uid
      d$ou_name <- daa.analytics::get_ou_name(d$ou_uid)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching DAA Indicator data"))
      d$daa_indicator_data <-
        daa.analytics::get_daa_data(ou_uid = d$ou_uid,
                                    d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Adorning DAA Indicator data"))
      d$daa_indicator_data %<>% daa.analytics::adorn_daa_data(.)
      d$datim_timestamp <- lubridate::now("UTC")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching site attribute data"))
      d$attribute_data <-
        daa.analytics::get_attribute_table(ou_uid = d$ou_uid,
                                           d2_session = d$d2_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching GeoAlign data"))
      d$data_availability <-
        daa.analytics::get_data_availability(geo_session = d$geo_session)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching PVLS and EMR data"))
      # TODO Fix timestamps
      s3 <- paws::s3()
      aws_s3_bucket <- Sys.getenv("AWS_S3_BUCKET")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Fetching category option combo metadata"))
      d$coc_metadata <-
        daa.analytics::get_coc_metadata(s3 = s3,
                                        aws_s3_bucket = aws_s3_bucket,
                                        last_update = NULL,
                                        folder = "data")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Fetching data element metadata"))
      d$de_metadata <-
        daa.analytics::get_de_metadata(s3 = s3,
                                       aws_s3_bucket = aws_s3_bucket,
                                       last_update = NULL,
                                       folder = "data")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Fetching organisation unit metadata"))
      d$ou_metadata <-
        daa.analytics::get_ou_metadata(s3 = s3,
                                       aws_s3_bucket = aws_s3_bucket,
                                       last_update = NULL,
                                       folder = "data")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625,
                         detail = ("Generating organisation unit hierarchy"))
      d$ou_hierarchy <- daa.analytics::create_hierarchy(d$ou_metadata)
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching period metadata"))
      d$pe_metadata <-
        daa.analytics::get_pe_metadata(s3 = s3,
                                       aws_s3_bucket = aws_s3_bucket,
                                       last_update = NULL,
                                       folder = "data")
      Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Fetching PVLS and EMR data"))
      d$pvls_emr <-
        daa.analytics::get_pvls_emr_table(s3 = s3,
                                          aws_s3_bucket = aws_s3_bucket,
                                          last_update = NULL,
                                          folder = "data")
      d$s3_timestamp <- s3_timestamp()

      shiny::incProgress(0.0625, detail = ("Adorning PVLS and EMR data"))
      d$pvls_emr %<>%  adorn_pvls_emr(pvls_emr = .,
                                      coc_metadata = d$coc_metadata,
                                      de_metadata = d$de_metadata,
                                      pe_metadata = d$pe_metadata)
      Sys.sleep(0.5)

      # shiny::incProgress(0.0625, detail = ("Checking data integrity: nulls"))
      # # TODO move this code over to datimutils package calls when available
      # tryCatch({
      #   d$integrity_nulls <-
      #     datapackr::api_sql_call("EfzMWuAlAOf", d2_session = d$d2_session)
      # }, error = function(e) {
      #   d$integrity_nulls <-
      #     data.frame(country_name = d$ou_name,
      #                message= glue::glue("An error occurred. No data \\
      #                                integrity checks could be retrieved."))
      # })
      # Sys.sleep(0.5)
      #
      # shiny::incProgress(0.0625,
      #                    detail = ("Checking data integrity: duplicates"))
      # # TODO move this code over to datimutils package calls when available
      # tryCatch({
      #   d$integrity_duplicates <-
      #     datapackr::api_sql_call("pZLyCkMVQG3", d2_session = d$d2_session)
      # }, error = function(e) {
      #   d$integrity_duplicates <-
      #     data.frame(country_name = d$ou_name,
      #                message= glue::glue("An error occurred. No data \\
      #                                integrity checks could be retrieved."))
      # })
      # Sys.sleep(0.5)

      shiny::incProgress(0.0625, detail = ("Merging datasets"))
      d$combined_data <- daa.analytics::combine_data(
        daa_indicator_data = d$daa_indicator_data,
        ou_hierarchy = d$ou_hierarchy,
        pvls_emr = d$pvls_emr,
        attribute_data = d$attribute_data)
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
      # shinyjs::hide("update_note")
      shinyjs::disable("save_con_graph")
      shinyjs::disable("integrity_radio_input")
      shinyjs::disable("save_pivot")
      shinyjs::disable("download_workbook")
      shinyjs::disable("download_raw")
      return(NULL)
    } else {
      shinyjs::show("hr-toggle")
      shinyjs::show("vz_de_input")
      shinyjs::show("vz_pe_input")
      # shinyjs::show("update_note")
      shinyjs::enable("save_con_graph")
      shinyjs::enable("integrity_radio_input")
      shinyjs::enable("save_pivot")
      shinyjs::enable("download_workbook")
      shinyjs::enable("download_raw")
      return(d)
    }
  }
}

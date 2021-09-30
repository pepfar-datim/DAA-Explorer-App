shinyServer(function(input, output, session) {
  # Reactive Values -----------------------------------------------------------

  ready <- reactiveValues(ok = FALSE)

  user_input <- reactiveValues(authenticated = FALSE,
                               authorized = FALSE,
                               ou_uid = NULL,
                               d2_session = NULL,
                               geo_session = NULL)

  filter_values <- reactiveValues(vz_de_filter = NULL,
                                  vz_pe_filter = NULL,
                                  wb_analysis_de_filter = NULL,
                                  wb_analysis_pe_filter = NULL,
                                  wb_raw_de_filter = NULL,
                                  wb_raw_pe_filter = NULL,
                                  integrity_radio = NULL)

  timestamps <- reactiveValues(s3 = NULL,
                               datim = NULL)

  data <- reactiveValues(gg_con = c(),
                         tb_pivot = c())

  # Reactives -----------------------------------------------------------------

  analysis_data <- reactive({
    d <- fetch(authenticated = user_input$authenticated,
               ready = ready,
               ou_uid = input$ou,
               d2_session = user_input$d2_session,
               geo_session = user_input$geo_session)
    if (is.null(d)) {
      ready$ok <- FALSE
    # } else {
    #   timestamps$s3 <- purrr::pluck(d, "s3_timestamp")
    #   timestamps$datim <- purrr::pluck(d, "datim_timestamp")
    }
    return(d)
  })

  # refreshed <- reactive({
  #   refresh_list <- list(s3 = timestamps$s3, datim = timestamps$datim)
  #   return(refresh_list)
  # })

  # Observer Reactivity -------------------------------------------------------

  observeEvent(input$fetch, {
    shinyjs::disable("ou")
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    # d <- analysis_data()
  })

  observeEvent(input$reset_input, {
    shinyjs::enable("ou")
    shinyjs::enable("fetch")
    shinyjs::hide("hr-toggle")
    shinyjs::hide("vz_de_input")
    shinyjs::hide("vz_pe_input")
    # shinyjs::hide("update_note")
    shinyjs::disable("save_con_graph")
    shinyjs::disable("integrity_radio_input")
    shinyjs::disable("save_pivot")
    shinyjs::disable("download_workbook")
    shinyjs::disable("download_raw")
    shinyjs::disable("reset_input")
    ready$ok <- FALSE
  })

  observeEvent(input$logout, {
    flog.info(paste0("User ",
                     user_input$d2_session$me$userCredentials$username,
                     " logged out."))
    ready$ok  <-  FALSE
    user_input$authenticated  <-  FALSE
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    user_input$geo_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })

  observeEvent(input$login_button, {
    base_url <- Sys.getenv(input$user_type)
    tryCatch({
      datimutils::loginToDATIM(username = input$user_name,
                               password = input$password,
                               base_url = base_url,
                               d2_session_envir = parent.env(environment()))
    },
    #This function throws an error if the login is not successful
    error = function(e) {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      futile.logger::flog.info(
        paste0("User ", input$user_name, " login failed."),
        name = "daa-analysis")
    })

    if (exists("d2_default_session")) {
      user_input$authenticated <- TRUE
      user_input$d2_session <- d2_default_session$clone()

      # Checks users credentials to make sure they are authorized to use app
      user_input$authorized <-
        credential_check(d2_session = user_input$d2_session)

      if (user_input$authorized == TRUE) {
        # Logs into GeoAlign
        datimutils::loginToDATIM(base_url = Sys.getenv("GEOALIGN_URL"),
                                 username = Sys.getenv("GEOALIGN_USERNAME"),
                                 password = Sys.getenv("GEOALIGN_PASSWORD"),
                                 d2_session_envir = parent.env(environment()))
        user_input$geo_session <- d2_default_session$clone()

        futile.logger::flog.info(
          paste0("User ", input$user_name, " logged in."),
          name = "daa-analysis")
      } else {
        futile.logger::flog.info(
          paste0("User ", input$user_name, " not authorized."),
          name = "daa-analysis")
      }
    }
  })

  observeEvent(input$ou, {
    user_input$ou_uid <- input$ou
  })
  observeEvent(input$vz_de_input, {
    filter_values$vz_de_filter <- input$vz_de_input
  })
  observeEvent(input$vz_pe_input, {
    filter_values$vz_pe_filter <- input$vz_pe_input
  })
  observeEvent(input$wb_analysis_de_input, {
    filter_values$wb_analysis_de_filter <- input$wb_analysis_de_input
  })
  observeEvent(input$wb_analysis_pe_input, {
    filter_values$wb_analysis_pe_filter <- input$wb_analysis_pe_input
  })
  observeEvent(input$wb_raw_de_input, {
    filter_values$wb_raw_de_filter <- input$wb_raw_de_input
  })
  observeEvent(input$wb_raw_pe_input, {
    filter_values$wb_raw_pe_filter <- input$wb_raw_pe_input
  })
  observeEvent(input$integrity_radio_input, {
    filter_values$integrity_radio <- input$integrity_radio_input
  })
  observeEvent(input$pivot_data,{
    data$tb_pivot <- extract_pivot_data(input$pivot_data)
  })

  # App UI --------------------------------------------------------------------

  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      callModule(ui_login, "ui")
    } else if (user_input$authorized == FALSE) {
      callModule(ui_unauth, "ui")
    } else {
      callModule(ui_main, "main",
                 d2_session = user_input$d2_session) #,
                 # refreshed = refreshed())
    }
  })

  # Table and Graph Outputs ---------------------------------------------------

  ## Data Availability Table --------------------------------------------------
  output$data_availability <- gt::render_gt(
    expr = analysis_data() %>%
      data_availability_table(),
    height = px(700),
    width = "70%"
  )

  ## Indicator Table ----------------------------------------------------------
  output$indicator_table <- gt::render_gt(
    expr = analysis_data() %>%
      country_summary(filter_values = filter_values) %>%
      indicator_table_rendering(),
    height = px(650),
    width = px(900)
  )

  ## Concordance Graph --------------------------------------------------------
  output$concordance_graph <- renderPlot({
    gg_con <- analysis_data() %>%
      country_summary(filter_values = filter_values) %>%
      concordance_chart()

    data$gg_con <- gg_con

    gg_con
  })

  ## Reporting Graph --------------------------------------------------------
  output$reporting_graph <- renderPlot({
    gg_rprt <- analysis_data() %>%
      matching_summary(filter_values = filter_values) %>%
      site_reporting_graph()

    data$gg_rprt <- gg_rprt

    gg_rprt
  })

  ## Scatterplot Graph --------------------------------------------------------
  output$interactive_scatter <- renderPlotly({
    gg_scatter <- analysis_data() %>%
      interactive_scatter(filter_values = filter_values)

    data$gg_scatter <- gg_scatter

    gg_scatter
  })

  ## Site Level Data Table ----------------------------------------------------
  output$site_table <- DT::renderDataTable(
    analysis_data() %>%
      site_table_data(filter_values = filter_values)
  )

  ## Integrity Check Table ----------------------------------------------------
  output$integrity_table <-
    DT::renderDataTable(generate_integrity_table(d = analysis_data(),
                                                 filter_values = filter_values))

  ## Pivot Table --------------------------------------------------------------

  output$pivot <- renderRpivotTable(moh_pivot(analysis_data()))

  # Download Handlers ---------------------------------------------------------

  ## Analysis Workbooks -------------------------------------------------------
  output$download_workbook <- downloadHandler(
    filename = wb_filename(d = analysis_data(),
                           type = "analysis"),
    content = function(file) {
      df <- analysis_data() %>%
        adorn_export_data() %>%
        table_filter(de_filter = filter_values$wb_analysis_de_filter,
                     pe_filter = filter_values$wb_analysis_pe_filter)
      wb <- wb_filecontent(df = df, file = file)
      return(wb)
    }
  )

  ## Raw Workbooks ------------------------------------------------------------
  output$download_raw <- downloadHandler(
    filename = wb_filename(d = analysis_data(),
                           type = "raw"),
    content = function(file) {
      d <- analysis_data() %>%
        adorn_export_data() %>%
        table_filter(de_filter = filter_values$wb_raw_de_filter,
                     pe_filter = filter_values$wb_raw_pe_filter)
      raw_file <- write.csv(d, file)
      return(raw_file)
    }
  )

  ## Save Concordancy Graph ---------------------------------------------------
  output$save_con_graph <- downloadHandler(
    filename =
      paste0(analysis_data() %>% purrr::pluck("ou_name"), "_graph.png"),
    content = function(file) {
      ggplot2::ggsave(file, plot = data$gg_con)
    }
  )

  ## Save Pivot Table ---------------------------------------------------------
  output$save_pivot <- downloadHandler(
    filename =
      paste0(analysis_data() %>% purrr::pluck("ou_name"), "_pivot_data.csv"),
    content = function(file) {
      write.csv(data$tb_pivot, file = file)
    }
  )
})

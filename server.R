library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(magrittr)
library(rpivotTable)
library(gt)
library(DT)
shinyServer(function(input, output, session) {
  # Reactive Values -----------------------------------------------------------

  ready <- reactiveValues(ok = FALSE) # indication of whether user has pushed the "Get Data" button

  user_input <- reactiveValues(authenticated = FALSE, # Whether a user has a valid DATIM account
                               authorized = FALSE, # Whether a user is authorized to access this application
                               ou_uid = NULL, # Organisation Unit UID for the country selected from the dropdown menu by the user
                               d2_session = NULL, # DATIM session object (see `datimutils` docs for more information)
                               geo_session = NULL) # GeoAlign session object (see `datimutils` docs for more information)

  filter_values <- reactiveValues(vz_de_filter = NULL, # Visualization Data Element (Indicator) Filter, located in sidebar
                                  vz_pe_filter = NULL, # Visualization Period (Fiscal Year) Filter, located in the sidebar
                                  wb_de_filter = NULL, # Raw CSV file Data Element (Indicator) Filter
                                  wb_pe_filter = NULL, # Raw CSV file Period (Fiscal Year) Filter
                                  integrity_radio = NULL) # Radio button for selecting which data integrity check to display

  data <- reactiveValues(gg_con = c(), # ggplot2 graph data for the Concordancy Graph
                         tb_pivot = c()) # rpivotTable data based on the current view in the pivot table

  # Reactives -----------------------------------------------------------------

  analysis_data <- reactive({
    # Run the fetch function to attempt to obtain data from DATIM and GeoAlign
    d <- fetch(authenticated = user_input$authenticated,
               ready = ready,
               ou_uid = input$ou,
               d2_session = user_input$d2_session,
               geo_session = user_input$geo_session)

    # If no data was returned, switch "Ready" indication back to False
    if (is.null(d)) {
      ready$ok <- FALSE
    }

    # Return the `d` list object with data

    return(d)
  })

  # Observer Reactivity -------------------------------------------------------

  # Waits for "Get Data" button to be pushed by user, then disables "Get Data" button and country dropdown menu
  # Also switched status of "Ready" to TRUE
  observeEvent(input$fetch, {
    shinyjs::disable("ou")
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    # d <- analysis_data()
  })

  # Waits for user to hit the "Reset" button, then re-enables the "Get Data" button and the country dropdown menu
  # Also disables or hides all UI elements associated with tables, graphs, and downloads
  # Switches status of "Ready" to FALSE
  observeEvent(input$reset_input, {
    shinyjs::enable("ou")
    shinyjs::enable("fetch")
    shinyjs::hide("hr-toggle")
    shinyjs::hide("vz_de_input")
    shinyjs::hide("vz_pe_input")
    shinyjs::disable("save_con_graph")
    shinyjs::disable("integrity_radio_input")
    shinyjs::disable("save_pivot")
    shinyjs::disable("download_raw")
    shinyjs::disable("reset_input")
    ready$ok <- FALSE
  })

  # Waits for user to hit the "Logout" button then resets app
  # Wipes out all session objects for active logins
  # Changes status of "authorized" and "authenticated" to FALSE
  # Changes status of "Ready" to FALSE
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
    base_url <- Sys.getenv("DATIM_URL")
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
        # Logs into GeoAlign using stored credentials for the app
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

  # Observes country dropdown menu and
  # updates `user_input$ou_uid` reactive value to match `input$ou` value
  observeEvent(input$ou, {
    user_input$ou_uid <- input$ou
  })

  # Observes indicator filter in sidebar and
  # updates `filter_values$vz_de_filter` reactive value to match `input$vz_de_input` value
  observeEvent(input$vz_de_input, {
    filter_values$vz_de_filter <- input$vz_de_input
  })

  # Observes fiscal year filter in sidebar and
  # updates `filter_values$vz_pe_filter` reactive value to match `input$vz_pe_input` value
  observeEvent(input$vz_pe_input, {
    filter_values$vz_pe_filter <- input$vz_pe_input
  })

  # Observes indicator filter on the downloads tab and
  # updates `filter_values$wb_de_filter` reactive value to match `input$wb_de_input` value
  observeEvent(input$wb_de_input, {
    filter_values$wb_de_filter <- input$wb_de_input
  })

  # Observes fiscal year filter on the downloads page and
  # updates `filter_values$wb_pe_filter` reactive value to match `input$wb_pe_input` value
  observeEvent(input$wb_pe_input, {
    filter_values$wb_pe_filter <- input$wb_pe_input
  })

  # Observes the radio button on the integrity check tab and
  # updates `filter_values$integrity_radio` reactive value to match `input$integrity_radio_input` value
  observeEvent(input$integrity_radio_input, {
    filter_values$integrity_radio <- input$integrity_radio_input
  })

  # Observes pivot table and
  # updates `data$tb_pivot` reactive value to match `input$pivot_data` value
  observeEvent(input$pivot_data,{
    data$tb_pivot <- extract_pivot_data(input$pivot_data)
  })

  # App UI --------------------------------------------------------------------

  # Progresses a user through various UI screens based on whether
  # they have successfully logged into their DATIM account and
  # whether they are authorized to access this app.
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) { # If a user is not authenticated, show them the login screen
      callModule(ui_login, "ui")
    } else if (user_input$authorized == FALSE) { # If a user is not authorized, show them the "unauthorized" screen
      callModule(ui_unauth, "ui")
    } else { # If a user is both authenticated and authorized, show them the main app content
      callModule(ui_main, "main",
                 d2_session = user_input$d2_session)
    }
  })

  # Table and Graph Outputs ---------------------------------------------------

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
    filename = function() {
      d <- analysis_data()
      if (is.null(d) || is.null(d$combined_data)) {
        return("no_data.txt")
      }

      date <- base::format(Sys.time(), "%Y%m%d_%H%M%S")
      ou_name <- d$ou_name

      name <- paste0(paste(date, ou_name, "raw_data", sep = "_"), ".csv")

      return(name)
    },
    content = function(file) {
      d <- analysis_data() %>%
        adorn_export_data() %>%
        table_filter(de_filter = filter_values$wb_de_filter,
                     pe_filter = filter_values$wb_pe_filter)
      raw_file <- write.csv(d, file)
      return(raw_file)
    }
  )



  ## Save Concordancy Graph ---------------------------------------------------
  output$save_con_graph <- downloadHandler(
    filename = function(){
      d <- analysis_data()
      ou_name <- d$ou_name
      name <- paste0(paste(ou_name, "_graph.png"))
      return(name)
    },

    content = function(file) {
      ggplot2::ggsave(file, plot = data$gg_con)
    }
  )

  ## Save Pivot Table ---------------------------------------------------------
  output$save_pivot <- downloadHandler(
    filename = function(){
      d <- analysis_data()
      ou_name <- d$ou_name
      name <- paste0(paste(ou_name, "_pivot_data.csv"))
      return(name)
    },

    content = function(file) {
      write.csv(data$tb_pivot, file = file)
    }
  )
})

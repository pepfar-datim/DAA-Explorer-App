pacman::p_load(shiny, shinyjs, shinyWidgets, futile.logger, httr, jsonlite,
               plyr, dplyr, tidyr, readr, magrittr, purrr, stringr, glue, tibble, 
               data.table, DT, ggplot2, scales, RColorBrewer, gt, rpivotTable,
               rvest, paws, htmlwidgets, datimutils, doMC)

source("./utils.R")
source("./visuals.R")

shinyServer(function(input, output, session) {
  # SETS APP VERSION VARIABLE -------------------------------------------
  app_info <- list(version = "2021.0.1")

  # REACTIVE VALUES -----------------------------------------------------

  ready <- reactiveValues(ok = FALSE)

  user_input <- reactiveValues(authenticated = FALSE,
                               d4u_user = FALSE,
                               user_orgunit = NULL,
                               country_level_user = FALSE,
                               authorized = FALSE,
                               status = "",
                               pivotData = NA,
                               handle = NULL,
                               geo_handle = NULL,
                               period_query = NULL,
                               datim_timestamp = NULL,
                               s3_timestamp = NULL)

  ou_info <- reactiveValues(ou_name = NULL,
                            country_level = NULL,
                            facility_level = NULL,
                            abbreviation = NULL)

  filters <- reactiveValues(wb_filter = NULL,
                            disc_indicator_filter = NULL,
                            site_indicator_filter = NULL,
                            site_period_filter = NULL)

  filter_options <- reactiveValues(ou_list = "",
                                   pe_list = "")

  data <- reactiveValues(disc_chart = NULL)

  # EVENT BUTTONS -------------------------------------------------------

  observeEvent(input$fetch, {
    shinyjs::disable("pe")
    shinyjs::disable("ou")
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    d <- analysis_data()
  })

  observeEvent(input$reset_input, {
    shinyjs::enable("pe")
    shinyjs::enable("ou")
    shinyjs::enable("fetch")
    shinyjs::disable("downloadInput")
    shinyjs::disable("download_wb")
    shinyjs::disable("download_raw")
    shinyjs::disable("reset_input")
    # shinyjs::hide("as_of")
    user_input$datim_timestamp <- NULL
    user_input$s3_timestamp <- NULL
    ready$ok <- FALSE
  })

  # FILTERS -------------------------------------------------------------

  observeEvent(input$userType, {

    if (input$userType == "DATIM") {
      user_input$d4u_user <- FALSE
    } else {
      user_input$d4u_user <- TRUE
    }

  })

  observeEvent(input$downloadInput, {

    filters$wb_filter <- input$downloadInput

  })

  observeEvent(input$discordanceInput, {

    filters$disc_indicator_filter <- input$discordanceInput

  })

  observeEvent(input$indicatorInput, {

    filters$site_indicator_filter <- input$indicatorInput

  })

  observeEvent(input$periodInput, {

    filters$site_period_filter <- input$periodInput

  })

  # PIVOT TABLE DATA -------------------------------------------------------

  pivotdf <- eventReactive(input$pivotData, {

    input$pivotData %>%
      xml2::read_html(.) %>%
      rvest::html_table(fill = TRUE) %>%
      .[[2]]

  })

  # FETCH FUNCTION ---------------------------------------------------------

  fetch <- function() {

    if (!user_input$authenticated | !ready$ok)  {
      return(NULL)
    } else {

      withProgress(message = "Fetching data", value = 0, {

        incProgress(0.111, detail = ("Fetching OU information"))
        ou_info$ou_name <- get_ou_name(input$ou, user_input$d4u_user)
        org_unit_info <- get_org_unit_info(ou_info$ou_name,
                                           user_input$d4u_user,
                                           user_input$handle)

        ou_info$country_level <- org_unit_info$country_level
        ou_info$facility_level <- org_unit_info$facility_level
        ou_info$abbreviation <- org_unit_info$abbreviation
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Fetching PVLS and EMR data"))

        # TODO Fix timestamps
        # Pulls timestamp from S3 bucket
        # user_input$s3_timestamp <- pvls_emr_timestamp()

        # TODO cache pvls and emr data and only pull if timestamp on S3 bucket
        # is more recent than when the data was last pulled
        pvls_emr <- get_pvls_emr_table()
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Joining with metadata"))
        pvls_emr <- pvls_emr_metadata(ou_info$ou_name,
                                      pvls_emr,
                                      ou_info$country_level)
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Fetching site attribute data"))
        site_attr <- get_attribute_table(input$ou,
                                         user_input$d4u_user,
                                         user_input$handle)
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Logging into GeoAlign"))
        geo_login <- dhis_login(Sys.getenv("GEOALIGN_URL"),
                                Sys.getenv("GEOALIGN_USERNAME"),
                                Sys.getenv("GEOALIGN_PASSWORD"))
        user_input$geo_handle <- geo_login$handle
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Fetching GeoAlign data"))
        geo <- get_geoalign_table(user_input$geo_handle)

        # TODO Fix GeoAlign query so that it returns dynamic list of periods
        # geo_data <- geo$geo_data
        # filter_options$pe_list <- geo$valid_years
        # user_input$period_query <- geo$period_list
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Fetching indicator data"))
        # TODO Fix period query so that years are dynamically brought in
        indicators <- get_indicators_table(input$ou,
                                           user_input$d4u_user,
                                           ou_info$facility_level,
                                           user_input$country_level_user,
                                           # user_input$period_query,
                                           user_input$handle)
        # TODO Fix timestamps
        # user_input$datim_timestamp <- Sys.time() %>%
        #   as.POSIXct(.) %>%
        #   format(., tz = "UTC", usetz = TRUE)
        Sys.sleep(0.5)

        incProgress(0.111, detail = ("Merging datasets"))
        analytics <- combine_data(indicators,
                                  pvls_emr,
                                  site_attr,
                                  ou_info$facility_level)
        Sys.sleep(0.5)

        incProgress(0.112, detail = ("Generating final dataset"))
        my_data <- list(indicators = indicators,
                        pvls_emr = pvls_emr,
                        site_attr = site_attr,
                        analytics = analytics,
                        geo_data = geo) # Remove and replace when fixed
                        # geo_data = geo_data)
        Sys.sleep(0.5)

      })

      if (is.null(my_data$emr) & is.null(my_data$indicators)) {
        sendSweetAlert(
          session,
          title = "Oops!",
          text = "Sorry, I could not find any data for you!"
        )

        shinyjs::enable("reset_input")

        ready$ok <- FALSE

        return(NULL)

      } else {

        shinyjs::enable("downloadInput")
        shinyjs::enable("download_wb")
        shinyjs::enable("download_raw")
        # shinyjs::show("as_of")
        shinyjs::enable("reset_input")

        return(my_data)
      }
    }
  }

  analysis_data <- reactive({
    fetch()
  })

  # UI FOR LOGIN SCREENS ------------------------------------------------------

  observeEvent(input$login_button, {
    is_logged_in <- FALSE
    if (user_input$d4u_user == FALSE) {
      baseurl <- Sys.getenv("DATIM_URL")
    } else {
      baseurl <- Sys.getenv("D4U_URL")
    }
    login_results <- dhis_login(baseurl,
                                input$user_name,
                                input$password)
    user_input$authenticated <- login_results$logged_in

    if (user_input$authenticated == TRUE) {
      # Saves handle to use for future DATIM or DATIM4U API access
      user_input$handle <- login_results$handle

      # Saves user org unit affiliation
      user_input$user_orgunit <- login_results$org_unit

      # Indicates whether user is global or country level user.
      user_input$country_level_user <- login_results$country_level_user

      # Checks users credentials to make sure they are authorized to use app
      user_input$authorized <- credential_check(login_results$user_groups,
                                                login_results$user_roles)

      # Pulls list of OUs that user will be able to access data for
      filter_options$ou_list <- get_user_ous(user_input$user_orgunit)
      flog.info(paste("User", input$datim_user_name,
                      input$user_type, "logged in.", sep = " "),
                name = "daa-analysis")
    } else {
        sendSweetAlert(
          session,
          title = "Login failed",
          text = "Please check your username/password!",
          type = "error")
        flog.info(paste0("User", input$datim_user_name, input$user_type,
                         "login failed.", sep = " "),
                  name = "daa-analysis")
      }
    })

  output$ui_login <- renderUI({
    wellPanel(fluidRow(
      HTML('<center><img src="pepfar.png"></center>'),
      h3("Welcome to the PEPFAR-MoH Data Alignment Activity Analysis app.",
         align = "center"),
      h4(paste0("Please select whether you are a DATIM or DATIM4U user and",
         " login with your credentials:"),
         align = "center")
    ),
    fluidRow(
      selectInput("userType", "User Type:",
                  choices = c("DATIM", "DATIM4U"),
                  selected = "DATIM"),
      textInput("user_name", "Username: ", width = "600px"),
      passwordInput("password", "Password:", width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })

  output$ui_unauthorized <- renderUI({
    wellPanel(
      fluidRow(
        HTML('<center><img src="pepfar.png"></center>'),
        h3("Welcome to the PEPFAR-MoH Data Alignment Activity Analysis app.",
           align = "center"),
        h4("You are not authorized to access this app",
           align = "center"),
        wellPanel(style = "background-color:#FDFDFD",
                  fluidRow(
                    align = "center",
                    "Access to this app is based upon your DATIM
                    credentials. Superusers, Global Users, and users
                    with access to MoH data in DATIM are allowed to
                    access this app. If you believe that you should
                    have access to this app, please submit a DATIM
                    support ticket." %>%
                      stringr::str_replace_all("[\r\n\t ]+", " ")))
      ))
  })

  # MAIN APP UI -------------------------------------

  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for DATIM login page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("ui_login"),
          uiOutput("pass")
        )
      ))
    } else if (user_input$authorized == FALSE) {
      ##### UI code for Unauthorized user page
      fluidPage(fluidRow(
        column(
          width = 2,
          offset = 5,
          br(),
          br(),
          br(),
          br(),
          uiOutput("ui_unauthorized"),
          uiOutput("pass")
        )
      ))
    } else {
      wiki_url <- a("Data Alignment Support Site",
                    href = ("https://datim.zendesk.com/hc/en-us/categories/
                            360000927432-PEPFAR-MoH-Data-Alignment-Activity" %>%
                              stringr::str_replace_all("[\r\n\t ]", "")),
                    target = "_blank")

      fluidPage(tags$head(
        tags$style(
          ".shiny-notification {
        position: fixed;
        top: 10%;
        left: 33%;
        right: 33%;}"
        )
      ),
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "side-panel",
          tagList(wiki_url),
          tags$hr(),
          selectInput("ou", "Operating Unit",
                      filter_options$ou_list),
          actionButton("fetch", "Get Data"),
          tags$hr(),
          "Download Analysis Workbooks",
          disabled(selectInput("downloadInput", "Choose a dataset:",
                               choices = c("HTS_TST", "PMTCT_STAT", "PMTCT_ART",
                                           "TB_PREV", "TX_CURR", "TX_NEW")),
                   downloadButton("download_wb", "Download",
                                  style = "width:100%;text-align: left;"),
                   tags$hr(),
                   downloadButton("download_raw", "Raw data",
                                  style = "width:100%;text-align: left;")),
          tags$hr(),
          # TODO Fix timestamps
          # Conditional panel that only displays when data has been pulled
          # and displayed "as of" dates of data being displayed in the app.
          # hide(p(id = "as_of",
          #        HTML(paste("PVLS and EMR data is as of:",
          #                   user_input$s3_timestamp,
          #                   "All other data is as of:",
          #                   user_input$datim_timestamp,
          #                   sep = "<br/>")))),
          # tags$hr(),
          disabled(actionButton("reset_input", "Reset Inputs")),
          tags$hr(),
          p(id = "update_note",
            HTML(paste0("<i>Please note that PVLS and EMR data ",
                        "are refreshed from DATIM daily ",
                        "at 8am EST. ",
                        "All other data is refreshed at time ",
                        " of app usage.</i>"))),
          p(id = "app_version", HTML(paste0("<b>App version ",
                                            app_info$version,
                                            "</b>"))),
          width = 2
        ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel(title = "Indicator Mapping",
                   gt::gt_output("data_availability")),
          tabPanel(title = "Indicator Analysis",
                   gt::gt_output("indicator_table")),
          tabPanel(title = "Discordance Graph",
                   downloadButton("save_disc_graph", "Save Graph"),
                   pickerInput("discordanceInput", "Indicator",
                               choices = c("HTS_TST", "PMTCT_STAT",
                                           "PMTCT_ART",
                                           "TB_PREV (FY2018-FY2019)",
                                           "TB_PREV (FY2020-Present)",
                                           "TX_CURR", "TX_NEW"),
                               selected = c("HTS_TST", "PMTCT_STAT",
                                            "PMTCT_ART",
                                            "TB_PREV (FY2018-FY2019)",
                                            "TB_PREV (FY2020-Present)",
                                            "TX_CURR", "TX_NEW"),
                               options = list(`actions-box` = TRUE),
                               multiple = T),
                   plotOutput("discordance_graph")),
          tabPanel(title = "Site Alignment Analysis",
                   wellPanel(
                     fluidRow(
                       column(6,
                              pickerInput("indicatorInput", "Indicator",
                                          choices = c(
                                            "HTS_TST", "PMTCT_STAT",
                                            "PMTCT_ART",
                                            "TB_PREV (FY2018-FY2019)",
                                            "TB_PREV (FY2020-Present)",
                                            "TX_CURR", "TX_NEW"
                                          ),
                                          selected = "HTS_TST",
                                          options = list(`actions-box` = TRUE),
                                          multiple = T)),
                       column(6,
                              pickerInput("periodInput", "Period",
                                          choices = c("FY2020" = 2020,
                                                      "FY2019" = 2019,
                                                      "FY2018" = 2018),
                                          # TODO bring in dynamic list of
                                          # periods based on GeoAlign
                                          # choices = filter_options$pe_list,
                                          selected = 2020,
                                          options = list(`actions-box` = TRUE),
                                          multiple = T))
                     )),
                   hr(),
                   dataTableOutput("site_table")),
          tabPanel(title = "Pivot Table",
                   downloadButton("save_pivot", "Save to CSV"),
                   rpivotTableOutput({
                     "pivot"
                   })) #,
          # TODO add functionality to compare country data
          # tabPanel(title = "Country Comparison",
          #          plotOutput("country_comparison"))
        ))
      ))
    }
  })

  # TABLE AND GRAPH OUTPUTS -------------------------

  ## RENDER DATA AVAILABILITY TABLE -------------------------------------------
  output$data_availability <- gt::render_gt(

    expr = if (ready$ok) {

      analysis_data() %>%
        purrr::pluck(., "geo_data") %>%
        dplyr::filter(CountryName == ou_info$ou_name) %>%
        dplyr::select(period,
                      indicator,
                      `Mapping` = hasDisagMapping,
                      `Results Data` = hasResultsData) %>%
        tidyr::nest(`Mapping`,
                    `Results Data`,
                    .key = "value_col") %>%
        tidyr::spread(key = period, value = value_col) %>%
        tidyr::unnest(names_sep = "_") %>%
        dplyr::select(which(colMeans(is.na(.)) != 1)) %>%
        gt::gt(rowname_col = "indicator") %>%
        gt::tab_spanner_delim(delim = "_")

    } else {
      NULL
    },
    height = px(700),
    width = "70%"

  )

  ## RENDER DISCORDANCE GRAPH -------------------------------------------------

  output$discordance_graph <- renderPlot({
    d <- analysis_data()
    ou_name <- ou_info$ou_name

    if (!inherits(d, "error") & !is.null(d)) {

      discordance_chart_data <- d %>%
        purrr::pluck(., "analytics") %>%
        dplyr::filter(indicator %in%
                        filters$disc_indicator_filter) %>%
        dplyr::full_join(.,
                         d$geo_data %>% dplyr::filter(.,
                                                 CountryName == ou_name),
                         by = c("indicator", "period")) %>%
        indicator_table_data()

      data$disc_chart <- discordance_chart(discordance_chart_data)

      data$disc_chart

    } else {
      NULL
    }
  })

  ## RENDER SITE DATA TABLE ---------------------------------------------------
  output$site_table <- DT::renderDataTable({

    d <- analysis_data()

    if (!inherits(d, "error") & !is.null(d)) {

      table_formatted <- d %>%
        purrr::pluck(., "analytics") %>%
        dplyr::filter(indicator %in% filters$site_indicator_filter,
                      period %in% filters$site_period_filter) %>%
        site_table_data(., input$ou, ou_info$facility_level)

      DT::datatable(table_formatted,
                    options = list(pageLength = 50),
                    rownames = FALSE) %>%
        DT::formatPercentage("Weighted difference", 3)

    } else {
      NULL
    }
  })

  ## RENDER INDICATOR TABLE ---------------------------------------------------
  output$indicator_table <- gt::render_gt(

    expr = if (ready$ok) {

      d <- analysis_data()

      d %>%
        purrr::pluck(., "analytics") %>%
        indicator_table_data(.) %>%
        dplyr::group_by(., indicator) %>%
        indicator_table_rendering(.)

    } else {
      NULL
    },
    height = px(650),
    width = px(900)

  )

  ## RENDER PIVOT TABLE ---------------------------------------------------

  output$pivot <- renderRpivotTable({

    d <- analysis_data()

    if (!is.null(d)) {

      if (is.null(d$analytics)) {
        return(NULL)
      }
      moh_pivot(d)

    } else {
      NULL
    }
  })

  # DOWNLOAD HANDLERS -------------------------------

  ## DOWNLOAD INDICATOR WORKBOOK ---------------------------------------------
  output$download_wb <- downloadHandler(
    filename = function() {

      name <- wb_filename(ou = ou_info$ou_name,
                          my_indicator = filters$wb_filter,
                          suffix = "analysis_workbook",
                          file_type = ".xlsx")

    },
    content = function(file) {

      d <- analysis_data()
      wb <- wb_filecontent(d, filters$wb_filter, file)
      return(wb)

    })

  ## DOWNLOAD RAW DATA WORKBOOK -----------------------------------------------
  output$download_raw <- downloadHandler(
    filename = function() {

      ou_name <- ou_info$ou_name
      name <- wb_filename(ou = ou_name,
                          suffix = "raw_data",
                          file_type = ".xlsx")

    },
    content = function(file) {

      d <- analysis_data()
      wb <- raw_filecontent(d, file)
      return(wb)

    })

  ## SAVE DISCORDANCY GRAPH ---------------------------------------------------
  output$save_disc_graph <- downloadHandler(
    filename = function() {
      paste0(ou_info$ou_name, "_graph.png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = data$disc_chart)
    }
  )

  ## SAVE PIVOT TABLE ---------------------------------------------------------
  output$save_pivot <- downloadHandler(
    filename = function() {

      ou_name <- ou_info$ou_name
      name <- wb_filename(ou = ou_name,
                          suffix = "pivot_data",
                          file_type = ".csv")

    },
    content = function(file) {

      data <- pivotdf()
      write.csv(data, file = file, row.names = F)

    })

})

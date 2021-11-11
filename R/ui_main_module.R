ui_main <- function(input, output, session, d2_session) { #, refreshed) {
  fluidPage(
    tags$head(
      tags$style(
      ".shiny-notification {
       position: fixed;
       top: 20%;
       left: 33%;
       right: 33%;}"
      )
      ),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        shinyjs::useShinyjs(),
        id = "side-panel",
        tagList(wiki_url),
        tags$hr(),
        selectInput("ou", "Select Operating Unit:",
                    choices = get_user_ous(d2_session)),
        actionButton("fetch", "Get Data"),
        actionButton("reset_input", "Reset"),
        div(id = "hr-toggle", HTML("<hr>")),
        pickerInput(inputId = "vz_de_input",
                    label = "Filters:",
                    choices = filter_options$de_list,
                    selected = filter_options$de_list,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput(inputId = "vz_pe_input",
                    label = NULL,
                    choices = filter_options$pe_list,
                    selected = filter_options$pe_list,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        # Conditional panel that only displays when data has been pulled
        # and displayed refresh dates of data being displayed in the app.
        # div(id = "update_note",
        #     HTML(glue::glue("<hr><p><strong>Data Refresh Times:</strong><br>\\
        #                     <em>PVLS and EMR data:<br>{refreshed$s3}<br>\\
        #                     All other data:<br>{refreshed$datim}\\
        #                     </em></p>"))),
        tags$hr(),
        actionButton("logout", "Logout"),
        tags$hr(),
        p(id = "app_version", HTML(glue::glue("Version: {app_info$version}")))),
      mainPanel(
        tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel(title = "Indicator Analysis",
                   div(id = "message-1", HTML(app_info$message)),
                   gt::gt_output("indicator_table")),
          tabPanel(title = "Reporting Rates",
                   div(id = "message-2", HTML(app_info$message)),
                   plotOutput("reporting_graph")),
          tabPanel(title = "Concordance Graph",
                   div(id = "message-3", HTML(app_info$message)),
                   downloadButton("save_con_graph", "Save Graph"),
                   plotOutput("concordance_graph")),
          tabPanel(title = "Site Data",
                   div(id = "message-4", HTML(app_info$message)),
                   dataTableOutput("site_table")),
          tabPanel(title = "Site Scatterplot",
                   div(id = "message-5", HTML(app_info$message)),
                   plotlyOutput("interactive_scatter")),
          # tabPanel(title = "Integrity Checks",
          #          radioButtons("integrity_radio_input",
          #                       label = "Select Data Integrity Test:",
          #                       choices = filter_options$integrity_list,
          #                       selected = filter_options$integrity_selected),
          #          dataTableOutput("integrity_table")),
          tabPanel(title = "Pivot Table",
                   div(id = "message-6", HTML(app_info$message)),
                   downloadButton("save_pivot", "Save to CSV"),
                   rpivotTableOutput("pivot")),
          tabPanel(title = "Downloads",
                   div(id = "message-7", HTML(app_info$message)),
                   fluidRow(
                     column(
                       width = 3,
                       offset = 2,
                       br(),
                       HTML(glue::glue('<center><h4><strong>\\
                                Analysis Workbooks</strong></h4>\\
                                <p><emph>Select a single indicator to \\
                                download a pre-packaged Excel workbook \\
                                with pivot tables to quickly start \\
                                analyzing your data:</emph></p>
                                </center>')),
                       pickerInput(inputId = "wb_analysis_de_input",
                                   label = NULL,
                                   choices = filter_options$de_list,
                                   selected = "HTS_TST"),
                       pickerInput(inputId = "wb_analysis_pe_input",
                                   label = NULL,
                                   choices = filter_options$pe_list,
                                   selected = filter_options$pe_list,
                                   options = list(`actions-box` = TRUE),
                                   multiple = TRUE),
                       downloadButton(outputId = "download_workbook",
                                      label = "Download",
                                      style = "width:100%;text-align: left;")
                       ),
                     column(
                       width = 3,
                       offset = 2,
                       br(),
                       HTML(glue::glue('<center><h4><strong>\\
                                Raw Data Flat Files</strong></h4>\\
                                <p><emph>Select any or all indicators and \\
                                reporting years to download a CSV file of \\
                                your DAA data for offline analysis:</emph></p>\\
                                </center>')),
                       pickerInput(inputId = "wb_raw_de_input",
                                   label = NULL,
                                   choices = filter_options$de_list,
                                   selected = filter_options$de_list,
                                   options = list(`actions-box` = TRUE),
                                   multiple = TRUE),
                       pickerInput(inputId = "wb_raw_pe_input",
                                   label = NULL,
                                   choices = filter_options$pe_list,
                                   selected = filter_options$pe_list,
                                   options = list(`actions-box` = TRUE),
                                   multiple = TRUE),
                       checkboxInput(inputId = "wb_raw_uids_input",
                                     label = "Include UIDs for entire hierarchy\n(increases file size)",
                                     value = FALSE),
                       downloadButton(outputId = "download_raw",
                                      label = "Download",
                                      style = "width:100%;text-align: left;"))
                     ))
    ))
  )
  )
}

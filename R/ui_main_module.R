# This is the main UI for the app once a user has been authenticated and authorized on the previous login screen
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
        tags$hr(),
        actionButton("logout", "Logout"),
        tags$hr(),
        p(id = "app_version", HTML(glue::glue("Version: {app_info$version}"))),
        p(id = "last_update_version", HTML(glue::glue("Data Last Updated: {last_updated$updated_last} UTC")))),
      mainPanel(
        tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel(title = "Indicator Analysis",
                   gt::gt_output("indicator_table")),
          tabPanel(title = "Reporting Rates",
                   plotOutput("reporting_graph")),
          tabPanel(title = "Concordance Graph",
                   downloadButton("save_con_graph", "Save Graph"),
                   plotOutput("concordance_graph", height = "600px")),
          tabPanel(title = "Site Data",
                   dataTableOutput("site_table")),
          tabPanel(title = "Site Scatterplot",
                   plotlyOutput("interactive_scatter", height = "600px", width = "100%")),
          tabPanel(title = "Integrity Checks",
                   radioButtons("integrity_radio_input",
                                label = "Select Data Integrity Test:",
                                choices = filter_options$integrity_list,
                                selected = filter_options$integrity_selected),
                   dataTableOutput("integrity_table")),
          tabPanel(title = "Pivot Table",
                   downloadButton("save_pivot", "Save to CSV"),
                   rpivotTableOutput("pivot")),
          tabPanel(title = "Downloads",
                   HTML(glue::glue('<center><h4><strong>\\
                                Raw Data Flat Files</strong></h4>\\
                                <p><emph>Select any or all indicators and \\
                                reporting years to download a CSV file of \\
                                your DAA data for offline analysis:</emph></p>\\
                                </center>')),
                   pickerInput(inputId = "wb_de_input",
                               label = NULL,
                               choices = filter_options$de_list,
                               selected = filter_options$de_list,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE),
                   pickerInput(inputId = "wb_pe_input",
                               label = NULL,
                               choices = filter_options$pe_list,
                               selected = filter_options$pe_list,
                               options = list(`actions-box` = TRUE),
                               multiple = TRUE),
                   downloadButton(outputId = "download_raw",
                                  label = "Download",
                                  style = "width:100%;text-align: left;"))
        )
      ))
  )
}

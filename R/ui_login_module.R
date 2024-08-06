ui_login <- function(input, output, session) {
  fluidPage(fluidRow(
    column(
      width = 3,
      offset = 4,
      br(),
      wellPanel(
        fluidRow(HTML('<center><img src="pepfar.png"><h3>Welcome to the DAA Explorer app</h3><h4>Please login with your DATIM credentials:</h4></center>')),
        br(),
        fluidRow(div(align = "center", actionButton("login_button_oauth", "Log in with DATIM"))),
        tags$hr(),
        fluidRow(HTML('<div style="font-size:small;text-align: center;"><p>Version: 1.0.0</p></div>'))
      ),
      uiOutput("pass")
    )
  ))
}

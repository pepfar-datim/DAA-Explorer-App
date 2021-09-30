ui_unauth <- function(input, output, session) {
  ##### UI code for Unauthorized user page
  fluidPage(fluidRow(
    column(
      width = 5,
      offset = 4,
      br(),
      wellPanel(
        fluidRow(
          HTML('<center><img src="pepfar.png"></center>'),
          h3("Welcome to the DAA Explorer app.",
             align = "center"),
          h4("You are not authorized to access this app", align = "center"),
          wellPanel(style = "background-color:#FDFDFD",
                    fluidRow(
                      align = "center",
                      glue::glue("
                              Access to this app is based upon your DATIM \\
                              credentials. Superusers, Global Users, and \\
                              users with access to MoH data in DATIM are \\
                              allowed to access this app. If you believe that \\
                              you should have access to this app, please \\
                              submit a DATIM support ticket.
                              ")
                    )))),
      uiOutput("pass")
    )
  ))
}

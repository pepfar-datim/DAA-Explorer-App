ui_login <- function(input, output, session) {
  ##### UI code for DATIM login page
  fluidPage(fluidRow(
    column(
      width = 3,
      offset = 4,
      br(),
      wellPanel(
        fluidRow(HTML(glue::glue('<center><img src="pepfar.png">\\
                                <h3>Welcome to the<br>DAA Explorer app</h3>\\
                                <h4>Please login with your<br>\\
                                DATIM or DATIM4U credentials:</h5>
                                </center>'))),
        br(),
        fluidRow(div(align = "center",
                     textInput(inputId = "user_name",
                               label = NULL,
                               placeholder = "Username"),
                     passwordInput(inputId = "password",
                                   label = NULL,
                                   placeholder = "Password"),
                     radioButtons(inputId = "user_type",
                                  label = NULL,
                                  choices = c("DATIM user" = "DATIM_URL",
                                              "DATIM4U user" = "D4U_URL"),
                                  selected = "DATIM_URL",
                                  inline = TRUE),
                     actionButton("login_button", "Log in!"))),
      tags$hr(),
      fluidRow(
        HTML(glue::glue('<div style="font-size:small;text-align: center;"><p>\\
                        Version: {app_info$version}</p></div>')))
      ),
      uiOutput("pass")
    )
  ))
}

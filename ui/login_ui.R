login_ui <- function() {
    tagList(
        div(id = "login",
            wellPanel(textInput(inputId = "Username", label = "Username",
                                placeholder = "guest"),
                      passwordInput(inputId = "Password", label = "Password"),
                      br(),
                      actionButton(inputId = "Login", label = "Log in"),
                      br(),
                      uiOutput(outputId = "UsernameError", inline = TRUE),
                      br(),
                      uiOutput(outputId = "PasswordError", inline = TRUE)
                      )
            ),
        div(id = "license",
            fluidRow(column(width = 8, offset = 2,
                            includeMarkdown(paste0(getwd(), "/LICENSE.md"))))
            ),
        tags$style(type = "text/css",
                   "#login {font-size:12px; text-align:left; position:absolute; top:20%; left: 50%; margin-top: -100px; margin-left: -150px;}",
                   "#license {font-size:12px; text-align: left; position:absolute; top:55%; left: 0%;}"
                   )
        )
}  ## End of login_ui

users <- data.frame(
    Username = c("dmmar",  "guar",  "mhhl", "guest"),
    Password = c("11111", "22222", "33333",      "")
);

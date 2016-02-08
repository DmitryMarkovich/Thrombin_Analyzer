library(shiny);
## widget : a web element a user can interact with
## inputId : name for the widget for the developer (object name)
## label : widget name for the user

shinyUI(
    ui = fluidPage(
        headerPanel("Thrombin Analyzer"), ## App title
        ## titlePanel("Thrombin Analyzer"),

        sidebarLayout(
            sidebarPanel = sidebarPanel(
                fileInput(inputId = "cal.fname", accept = ("text/csv"),
                          label = h4("Calibration data file input")
                          ),
                numericInput(inputId = "cal.e0",
                             label = "Set Thrombin Calibrator concentration (nM):",
                             value = 100, min = 1, max = 500),
                numericInput(inputId = "cal.s0",
                             label = "Set FluCa concentration (uM):",
                             value = 454, min = 200, max = 500),
                selectInput(inputId = "cal.model",
                            label = h4("Select model to fit calibration signal"), 
                            ## choices = list("LateMM" = "LateMM", "LateExp" = "LateExp",
                            ##     "EarlyMM" = "EarlyMM", "LM" = "LM", "Auto" = "Auto"),
                            choices = c("LateMM", "LateExp", "EarlyMM", "LM", "Auto"),
                            selected = NULL
                            ),
                fileInput(inputId = "data.tg", accept = ("text/csv"),
                          label = h4("Thrombin generation data file input")
                          ),
                selectInput(inputId = "tg.model",
                            label = h4("Select model to fit thrombin generation signal"), 
                            choices = list("T0GammaInt" = 1, "GammaInt" = 2,
                                "2T0GammaInt" = 3, "T0Gamma", "Auto" = 4),
                            selected = 1),
                width = 4
            ),
            mainPanel = mainPanel(
                tabsetPanel(
                    id = "Signal type",
                    tabPanel(title = "Calibration signal",
                             ## h1("Plot of calibration signal", align = "left")
                             plotOutput(outputId = "cal.Plot"),
                             textOutput(outputId = "cal.model")
                             ## plotOutput(outputId = "PlotFit")
                             ),
                    tabPanel(title = "Thrombin generation signal"
                             ## h1("Plot of thrombin generation signal", align = "left")
                             ## plotOutput(outputId = "PlotTG")
                             ),
                    tabPanel(title = "Thrombogram"
                             ## h1("Plot of thrombin generation signal", align = "left")
                             ## plotOutput(outputId = "PlotThromb")
                             ),
                    tabPanel(title = "Parameters",
                             ## h1("Parameters from calibration experiment", align = "left")
                             dataTableOutput(outputId = "cal.parms")
                             )
                ),  ## End of tabsetPanel
                width = 8
            )  ## End of mainPanel
        ),  ## End of sidebarLayout
        title = "Thrombin Analyzer",
        responsive = NULL,
        theme = NULL
    ) ## End of Fluidpage
)  ## End of ShinyUI

    ## fluidRow(
    ##     column(width = 3,
    ##            h3("Buttons:"),
    ##            actionButton(inputId = "action", label = "Action button"),
    ##            br(),
    ##            br(),
    ##            submitButton(text = "Submit button", icon = NULL, width = NULL),
    ##            offset = 0
    ##            ),

    ##     column(width = 3,
    ##            h3("Single checkbox element:"),
    ##            checkboxInput(inputId = "checkbox", label = "Choice A", value = T),
    ##            checkboxInput(inputId = "checkbox", label = "Choice B", value = F)
    ##            ),

    ##     column(width = 3,
    ##            checkboxGroupInput(inputId = "checkGroup",
    ##                               label = h3("Checkbox group element:"),
    ##                               choices = list("Choice 01" = 10,
    ##                                   "Choice 02" = 27, "Choice 03" = 4),
    ##                               selected = 10)
    ##            ),

    ##     column(width = 3,
    ##            dateInput(inputId = "date",
    ##                      label = h3("Date input:"),
    ##                      value = "2015-09-03")
    ##            )
    ## ),  ## End of fluidRow

    ## fluidRow(
    ##     column(width = 3, dateRangeInput(inputId = "dates", label = h3("Date range:"))),
    ##     column(width = 3, fileInput(inputId = "file", label = h3("File input"))),
    ##     column(width = 3, h3("Help text:"),
    ##            helpText("Note: help text isn't a true widget, but it provides an easy way to add text to accompany other widgets.")),
    ##     column(width = 3, numericInput(inputId = "num", label = h3("Numeric input:"),
    ##                value = 404))
    ## ),  ## End of fluidRow

    ## fluidRow(column(width = 3,
    ##                 radioButtons(inputId = "radio", label = h3("Radio buttons:"),
    ##                              choices = list("Choice 1" = 1, "Choice 2" = 225,
    ##                                  "Choice 3" = 3), selected = 225)),
    ##          column(width = 3,
    ##                 selectInput(inputId = "select", label = h3("Select box:"),
    ##                             choices = list("Choice 1" = 1111, "Choice 2" = 2,
    ##                                 "Choice 3" = 3), selected = 1111)),
    ##          column(width = 3,
    ##                 sliderInput(inputId = "slider1", label = h3("Sliders:"),
    ##                             min = -20, max = 150, value = 0.1, animate = F),
    ##                 sliderInput(inputId = "slider2", "",
    ##                             min = 0, max = 100, value = c(25, 75))
    ##                 ),
    ##          column(width = 3,
    ##                 textInput(inputId = "text", label = h3("Text input:"),
    ##                           value = "..."))
    ##          )  ## End of fluidRow

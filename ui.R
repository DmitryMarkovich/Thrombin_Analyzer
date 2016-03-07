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
                h2("Calibration", align = "center"),
                fileInput(inputId = "cal.fname", accept = ("text/csv"),
                          label = h4("Calibration data file input")
                          ),
                selectInput(inputId = "cal.model",
                            label = h4("Select model to fit calibration signal"), 
                            choices = c("Auto", "LateMM", "LateExp", "EarlyMM", "LM",
                                "None"),
                            selected = "None"
                            ),
                h2("Thrombin generation", align = "center"),
                fileInput(inputId = "tg.fname", accept = ("text/csv"),
                          label = h4("Thrombin generation data file input")
                          ),
                selectInput(inputId = "tg.model",
                            label = h4("Select model to fit thrombin generation signal"), 
                            choices = c("Auto",
                                "LateExpT0GammaInt", "LateExpGammaInt",
                                "T0GammaInt", "GammaInt", "T0Gamma",
                                "Gamma", "None"),
                            selected = "None"),
                width = 4
            ),
            mainPanel = mainPanel(
                tabsetPanel(
                    id = "Signal type",
                    tabPanel(title = "Calibration signal",
                             ## h1("Plot of calibration signal", align = "left")
                             plotOutput(outputId = "cal.Plot"),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        ## h2("Place for residuals")
                                        plotOutput(outputId = "cal.PlotResid")
                                        ),
                                 column(width = 6,
                                        ## h2("Place for summary")
                                        htmlOutput(outputId = "cal.model")
                                        )
                                 )  ## End of fluidRow
                             ),
                    tabPanel(title = "Thrombin generation signal",
                             ## h1("Plot of thrombin generation signal", align = "left")
                             plotOutput(outputId = "tg.Plot"),
                             ## htmlOutput(outputId = "tg.model")
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        ## h2("Place for residuals")
                                        plotOutput(outputId = "tg.PlotResid")
                                        ),
                                 column(width = 6,
                                        ## h2("Place for summary")
                                        htmlOutput(outputId = "tg.model")
                                        )
                                 )  ## End of fluidRow
                             ),
                    tabPanel(title = "Thrombogram",
                             ## h1("Plot of thrombin generation signal", align = "left")
                             plotOutput(outputId = "tg.PlotDrv1")
                             ),
                    tabPanel(title = "Parameters",
                             ## h1("Parameters from calibration experiment", align = "left")
                             ## numericInput(inputId = "AAA",
                             ##              label = "Dummy input",
                             ##              value = 1, min = 0, max = Inf),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        numericInput(inputId = "cal.e0",
                                                     label = "Set Thrombin Calibrator concentration e0 (nM):",
                                                     value = 100, min = 0, max = Inf,
                                                     width = "100%")
                                        ),
                                 column(width = 6, offset = 0,
                                        numericInput(inputId = "cal.s0",
                                                     label = "Set FluCa concentration s0 (uM):",
                                                     value = 454, min = 0, max = Inf,
                                                     width = "100%")
                                        )
                                 ),
                             dataTableOutput(outputId = "cal.ShowParms"),
                             numericInput(inputId = "cal.CF",
                                          label = "Set calibration factor value CF (nM * min / a.u.):",
                                          value = 1, min = 0, max = Inf, width = "100%"),
                             dataTableOutput(outputId = "tg.ShowParms")
                             )
                ),  ## End of tabsetPanel
                width = 8
            )  ## End of mainPanel
        ),  ## End of sidebarLayout
        title = "Thrombin Analyzer",
        responsive = NULL,
        ## theme = "bootstrap.css"
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

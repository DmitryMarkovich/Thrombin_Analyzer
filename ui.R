library(shiny);

shinyUI(
    ui = fluidPage(
        headerPanel("Thrombin Analyzer"), ## App title

        sidebarLayout(
            sidebarPanel = sidebarPanel(
                h2("Calibration", align = "center"),
                fileInput(inputId = "cal.fname", accept = ("text/csv"),
                          label = h4("Calibration data file input")
                          ),
                selectInput(inputId = "cal.model",
                            label = h4("Select model to fit calibration signal"), 
                            choices = c("Auto", "LateMM", "LateExp", "EarlyMM",
                                "LM", "None"),
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
                h2("Demo signals", align = "center"),
                selectInput(inputId = "demo.signal",
                            label = h4("Select a demo signal to try out models on data!"), 
                            choices = c(
                                "Calibration - LM, EarlyMM", "Calibration - LateExp",
                                "Calibration - LateMM", "Calibration - Paper",
                                "Thrombin generation - Gamma, T0Gamma",
                                "Thrombin generation - GammaInt, T0GammaInt",
                                "Thrombin generation - LateExpGammaInt, LateExpT0GammaInt",
                                "Thrombin generation - Paper Control",
                                "Thrombin generation - Paper Green",
                                "Thrombin generation - Paper Red",
                                "None"
                                ),
                            selected = "None"),
                uiOutput(outputId = "demo.Download"),
                width = 4
            ),
            mainPanel = mainPanel(
                tabsetPanel(
                    id = "Signal type",
                    tabPanel(title = "Calibration signal",
                             plotOutput(outputId = "cal.Plot"),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        plotOutput(outputId = "cal.PlotResid")
                                        ),
                                 column(width = 6,
                                        htmlOutput(outputId = "cal.model")
                                        )
                                 )  ## End of fluidRow
                             ),
                    tabPanel(title = "Thrombin generation signal",
                             plotOutput(outputId = "tg.Plot"),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        plotOutput(outputId = "tg.PlotResid")
                                        ),
                                 column(width = 6,
                                        htmlOutput(outputId = "tg.model")
                                        )
                                 )  ## End of fluidRow
                             ),
                    tabPanel(title = "Thrombogram",
                             plotOutput(outputId = "tg.PlotDrv1")
                             ),
                    tabPanel(title = "Parameters",
                             fluidRow(
                                 column(width = 4, offset = 0,
                                        numericInput(inputId = "cal.e0",
                                                     label = "Set Thrombin Calibrator concentration e0 (nM):",
                                                     value = 100, min = 0, max = Inf,
                                                     width = "100%")
                                        ),
                                 column(width = 4, offset = 0,
                                        numericInput(inputId = "cal.s0",
                                                     label = "Set FluCa concentration s0 (uM):",
                                                     value = 454, min = 0, max = Inf,
                                                     width = "100%")
                                        ),
                                 column(width = 4, offset = 0,
                                        numericInput(inputId = "cal.CF",
                                                     label = "Set calibration factor value CF (nM * min / a.u.):",
                                                     value = 1, min = 0, max = Inf, width = "100%")
                                        )
                                 ),  ## End of fluidRow
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        h4("Parameters from calibration signal"),
                                        tableOutput(outputId = "cal.ShowParms")
                                        ),
                                 column(width = 6, offset = 0,
                                        h4("Parameters from thrombin generation signal"),
                                        tableOutput(outputId = "tg.ShowParms")
                                        )
                                 )  ## End of fluidRow
                             ),
                    tabPanel(title = "Demo signals",
                             fluidRow(
                                 column(width = 3,
                                        tableOutput(outputId = "demo.Show")
                                        ),
                                 column(width = 9,
                                        plotOutput(outputId = "demo.Plot")
                                        )
                                 )
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

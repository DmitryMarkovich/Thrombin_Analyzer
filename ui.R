library(shiny);
################################################################################
shinyUI(
    ui = fluidPage(
        tags$head(tags$style(".rightAlign{float:right;}")),
        headerPanel("Thrombin Analyzer"), ## App title
################################################################################
######################################## Sidebar layout
################################################################################
        sidebarLayout(
            sidebarPanel = sidebarPanel(
######################################## Dataset
                h2("Dataset", align = "center"),
                fluidRow(
                    column(width = 5, offset = 0,
                           fileInput(inputId = "dataset.fname", accept = ("text/csv"),
                                     label = h5("Load dataset file"))
                           ),
                    column(width = 4, offset = 0,
                           radioButtons(inputId = "dataset.show",
                                        label = h5("Show as"),
                                        choices = list("plot" = "plot",
                                            "text" = "text"),
                                        selected = "plot", inline = TRUE)
                           ),
                    column(width = 3, offset = 0,
                           actionButton(inputId = "dataset.analyze",
                                        label = h5("Analyze!"),
                                        inline = TRUE)
                           )
                    ),  ## End of FluidRow
                uiOutput(outputId = "dataset.Menu"),
######################################## Calibration
                h2("Calibration", align = "center"),
                fluidRow(
                    column(width = 5,
                           fileInput(inputId = "cal.fname", accept = ("text/csv"),
                                     label = h4("Load calibration data file"))
                           ),
                    column(width = 7,
                           selectInput(inputId = "cal.model",
                                       label = h4("Select model to fit calibration signal"), 
                                       choices = c("Auto", "LateMM", "LateExp", "EarlyMM",
                                           "LM", "None"),
                                       selected = "None")
                           )
                    ),  ## End of fluidRow
######################################## Thrombin generation
                h2("Thrombin generation", align = "center"),
                fluidRow(
                    column(width = 5,
                           fileInput(inputId = "tg.fname", accept = ("text/csv"),
                                     label = h4("Load thrombin generation data file"))
                           ),
                    column(width = 7,
                           selectInput(inputId = "tg.model",
                                       label = h4("Select model to fit thrombin generation signal"), 
                                       choices = c("Auto",
                                           "LateExpT0GammaInt", "LateExpGammaInt",
                                           "T0GammaInt2",
                                           "T0GammaInt", "GammaInt", "T0Gamma",
                                           "Gamma", "None"),
                                       selected = "None")
                           )
                    ),  ## End of fluidRow
######################################## Demo signals
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
                                "Synthetic - Calibration - LM",
                                "Synthetic - Calibration - EarlyMM",
                                "Synthetic - Calibration - LateExp",
                                "Synthetic - Calibration - LateMM",
                                "Synthetic - Thrombin generation - Gamma",
                                "Synthetic - Thrombin generation - T0Gamma",
                                "Synthetic - Thrombin generation - GammaInt",
                                "Synthetic - Thrombin generation - T0GammaInt",
                                "Synthetic - Thrombin generation - LateExpGammaInt",
                                "Synthetic - Thrombin generation - LateExpT0GammaInt",
                                "None"
                                ),
                            selected = "None"),
                uiOutput(outputId = "demo.Download"),
                width = 4
                ),
################################################################################
######################################## Main panel
################################################################################
            mainPanel = mainPanel(
                tabsetPanel(
                    id = "Tab type",
######################################## Dataset tab
                    tabPanel(title = "Dataset",
                             fluidRow(
                                 column(width = 12, offset = 0,
                                        uiOutput(outputId = "dataset.ShowAs")
                                        ),
                                 br(),
                                 column(width = 12, offset = 0,
                                        uiOutput(outputId = "dataset.DoAnalysis")
                                        )
                                 )  ## End of fluidRow
                             ),
######################################## Overlay tab
                    tabPanel(title = "Overlay",
                             fluidRow(
                                 column(width = 12, offset = 0,
                                        plotOutput(outputId = "dataset.Overlay")
                                        )
                                 )  ## End of fluidRow
                             ),
######################################## Calibration signal tab
                    tabPanel(title = "Calibration signal",
                             plotOutput(outputId = "cal.Plot"),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        plotOutput(outputId = "cal.PlotResid")
                                        ),
                                 column(width = 6,
                                        htmlOutput(outputId = "cal.model")
                                        )
                                 ),  ## End of fluidRow
                             uiOutput(outputId = "cal.SynthHint",
                                      class = "rightAlign")
                             ),
######################################## Thrombin generation signal tab
                    tabPanel(title = "Thrombin generation signal",
                             plotOutput(outputId = "tg.Plot"),
                             fluidRow(
                                 column(width = 6, offset = 0,
                                        plotOutput(outputId = "tg.PlotResid")
                                        ),
                                 column(width = 6,
                                        htmlOutput(outputId = "tg.model")
                                        )
                                 ),  ## End of fluidRow
                             uiOutput(outputId = "tg.SynthHint",
                                      class = "rightAlign")
                             ),
######################################## Thrombogram tab
                    tabPanel(title = "Thrombogram",
                             plotOutput(outputId = "tg.PlotDrv1")
                             ),
######################################## Parameters tab
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
                                 column(width = 4, offset = 0,
                                        h4("Parameters from calibration signal", align = "center"),
                                        tableOutput(outputId = "cal.ShowParms")
                                        ),
                                 column(width = 8, offset = 0,
                                        h4("Parameters for thrombin generation signal", align = "center"),
                                        column(width = 6, offset = 0,
                                               h4("From fitted model", align = "left"),
                                               tableOutput(outputId = "tg.ShowParms")
                                               ),
                                        column(width = 6, offset = 0,
                                               h4("Estimated numerically", align = "right"),
                                               tableOutput(outputId = "tg.ShowParmsNum")
                                               )
                                        )
                                 )  ## End of fluidRow
                             ),
######################################## Demo signals tab
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
################################################################################

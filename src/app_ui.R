app_ui <- function() {
    fluidPage(
        tags$head(tags$style(".rightAlign{float:right;}")),  ## right-align elements in main panel
        headerPanel("Thrombin Analyzer"), ## App title
################################################################################
################################################################################
######################################## Sidebar layout
################################################################################
################################################################################
        sidebarLayout(
            sidebarPanel = sidebarPanel(
############################################################
######################################## Login
############################################################
                fluidRow(
                    column(width = 6, align = "left",
                           uiOutput(outputId = "PrintUser", inline = TRUE)
                           ),
                    column(width = 6, align = "right",
                           actionButton(inputId = "Logout", label = "Log out")
                           )
                    ),
############################################################
######################################## Dataset
############################################################
                fluidRow(
                    h2("Dataset", align = "center"),
                    column(width = 4, offset = 0,
                           fileInput(inputId = "dataset.fname", accept = ("text/csv"),
                                     label = h5("Load dataset"))
                           ),
                    column(width = 4, offset = 0,
                           fileInput(inputId = "parms.fname", accept = ("csv"),
                                     label = h6("Load parameters"))
                           ),
                    column(width = 4, offset = 0,
                           fileInput(inputId = "res.fname", accept = (".RData"),
                                     label = h5("Load results"))
                           )
                    ),
                uiOutput(outputId = "dataset.Menu"),
############################################################
######################################## Calibration
############################################################
                h2("Calibration", align = "center"),
                fluidRow(
                    column(width = 5,
                           fileInput(inputId = "cal.fname", accept = ("text/csv"),
                                     label = h4("Load calibration data file"))
                           ),
                    column(width = 7,
                           uiOutput(outputId = "cal.Menu")
                           )
                    ),  ## End of fluidRow
############################################################
######################################## Thrombin generation
############################################################
                fluidRow(
                    column(width = 12, offset = 0,
                           h2("Thrombin generation", align = "center")
                           )
                    ),
                fluidRow(
                    column(width = 5,
                           fileInput(inputId = "tg.fname", accept = ("text/csv"),
                                     label = h4("Load thrombin generation data file"))
                           ),
                    uiOutput(outputId = "tg.Menu")
                    ),  ## End of fluidRow
############################################################
######################################## Demo signals
############################################################
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
                ),  ## End of sidebarPanel
################################################################################
################################################################################
######################################## Main panel
################################################################################
################################################################################
            mainPanel = mainPanel(
############################################################
######################################## Instructions
############################################################
                tabsetPanel(
                    id = "Tab type",
                    tabPanel(title = "Instructions",
                             fluidRow(column(width = 8, offset = 2,
                                             includeMarkdown("~/Coding/R/Shiny/Thrombin_Analyzer/README.md")))
                             ),
############################################################
######################################## Dataset
############################################################
                    navbarMenu(title = "Dataset",
                               tabPanel(title = "Dataset",
                                        fluidRow(
                                            column(width = 12, offset = 0,
                                                   tableOutput(outputId = "dataset.ShowAs")
                                                   ),
                                            column(width = 12, offset = 0, br()),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowLagtime")
                                                   ),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowETP",
                                                              brush = brushOpts(id = "plot_brush",
                                                                  direction = "xy")
                                                              )
                                                   ),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowPeak")
                                                   ),
                                            column(width = 12, offset = 0,
                                                   ## verbatimTextOutput("brush_info")
                                                   uiOutput("brush_info")
                                                   ),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowttPeak")
                                                   ),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowVelIndex")
                                                   ),
                                            column(width = 4, offset = 0,
                                                   plotOutput(outputId = "dataset.ShowAlpha2M_Level")
                                                   )
                                            ## column(width = 12, offset = 0,
                                            ##        uiOutput(outputId = "dataset.ShowLoadedResults")
                                            ##        )
                                            )  ## End of fluidRow
                                        ),
                               tabPanel(title = "Overlay",
                                        fluidRow(
                                            column(width = 12, offset = 0,
                                                   plotOutput(outputId = "dataset.PlotOverlay")
                                                   ),
                                            column(width = 12, offset = 0,
                                                   plotOutput(outputId = "dataset.PlotDrvOverlay")
                                                   ),
                                            column(width = 12, offset = 0, align = "center",
                                                   tableOutput(outputId = "dataset.ShowParmsOverlay")
                                                   )
                                            )  ## End of fluidRow
                                        ),
                               tabPanel(title = "Overlay details",
                                        fluidRow(
                                            ## column(width = 12, offset = 0,
                                            ##        plotOutput(outputId = "dataset.PlotOverlay")
                                            ##        ),
                                            column(width = 12, offset = 0,
                                                   plotOutput(outputId = "dataset.PlotResidOverlay")
                                                   ),
                                            column(width = 12, offset = 0, ## align = "center",
                                                   uiOutput(outputId = "dataset.ShowSmryOverlay")
                                                   )
                                            )  ## End of fluidRow
                                        )
                               ),  ## End of navbarMenu
############################################################
######################################## Calibration signal
############################################################
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
############################################################
######################################## Thrombin generation signal
############################################################
                    navbarMenu(title = "Thrombin generation signal",
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
                                        ## uiOutput(outputId = "tg.SynthHint",
                                        ##          class = "rightAlign")
                                        ),
                               tabPanel(title = "Thrombogram",
                                        plotOutput(outputId = "tg.PlotDrv1"),
                                        plotOutput(outputId = "tg.PlotDrv2")
                                        )
                               ),  ## End of navbarMenu
############################################################
######################################## Parameters
############################################################
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
                                               h4("From fitted model", align = "center"),
                                               tableOutput(outputId = "tg.ShowParms")
                                               ),
                                        column(width = 6, offset = 0,
                                               h4("Estimated numerically", align = "center"),
                                               tableOutput(outputId = "tg.ShowParmsNum")
                                               )
                                        )
                                 )  ## End of fluidRow
                             ),
############################################################
######################################## Demo signals
############################################################
                    tabPanel(title = "Demo",
                             fluidRow(
                                 column(width = 12,
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
        )  ## End of Fluidpage
}  ## End of app_ui

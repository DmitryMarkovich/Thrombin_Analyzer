library(shiny);
source("src/global_parameters.R");
source("src/plotting_functions.R");
source("src/numerical_derivatives.R");
## code here runs once when app is launched
## create environment e to store loaded data
e <- new.env(, parent = emptyenv());  ## ensures no accidental copies of data
e$data.cal <- NULL; e$data.tg <- NULL;
e$data.tg.drv1 <- NULL;
## create df to store parameters obtained after fitting
e$df.cal[] <- list(kCalDFPar, value, stderr, kCalDFDim); print(e$df.cal);
e$df.cal <- data.frame(Parameter = kCalDFPar, Value = rep(NA_real_, 7),
                       StdErr = rep(NA_real_, 7), Dimensionality = kCalDFDim);
################################################################################
shinyServer(
    func = function(input, output) { ## runs each time a user visits the app
        output$PlotCal <- renderPlot({ ## runs each time user changes a widget
            inFile <- input$data.cal;
            if (!is.null(inFile)) {
                e$data.cal <- read.table(file = inFile$datapath, header = TRUE,
                                         col.names = c("x", "y"), sep = " ");
                PlotSignal(e, signal.type = "Calibration");
            }
        })  ## End of output$PlotCal
        output$PlotTG <- renderPlot({ ## runs each time user changes a widget
            inFile <- input$data.tg;
            if (!is.null(inFile)) {
                e$data.tg <- read.table(file = inFile$datapath, header = TRUE,
                                        col.names = c("x", "y"), sep = " ");
                PlotSignal(e, signal.type = "Thrombin generation");
            }
        })  ## End of output$PlotTG
        output$PlotThromb <- renderPlot({ ## runs each time user changes a widget
            if (!is.null(e$data.tg)) {
                GetDrv1(e);
                PlotSignal(e, signal.type = "Thrombogram");
            }
        })  ## End of output$PlotThromb
        output$ParCalibr <- renderDataTable({
            e$df.cal;
        })  ## End of output$ParCalibr
    }  ## End of function
)  ## End of shinyServer
################################################################################

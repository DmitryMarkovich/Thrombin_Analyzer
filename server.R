library(shiny);
print(">> ################################################################################ <<");
    ## source("src/global_parameters.R");
    source("src/base_class.R");
source("src/cal_class.R");
## source("src/tg_class.R");
## source("src/plotting_functions.R");
## source("src/numerical_derivatives.R");
## source("src/models.R");
## code here runs once when app is launched
cal <- Cal$new();
## ## create environment e to store loaded data
## e <- new.env(, parent = emptyenv());  ## ensures no accidental copies of data
## e$data.cal <- NULL; e$data.cal.smry <- NULL; e$data.cal.fit <- NULL;
## e$data.tg <- NULL; e$data.tg.smry <- NULL;
## e$data.tg.drv1 <- NULL;
## ## create df to store parameters obtained after fitting
## e$df.cal[] <- list(kCalDFPar, value, stderr, kCalDFDim);
## e$df.cal <- data.frame(Parameter = kCalDFPar, Value = rep(NA_real_, 7),
##                        StdErr = rep(NA_real_, 7), Dimensionality = kCalDFDim);
################################################################################
shinyServer(
    func = function(input, output) { ## runs each time a user visits the app
        cal.fname <- reactive({
            return(input$cal.fname);
        })
        output$cal.Plot <- renderPlot({ ## runs each time user changes a widget
            if (!is.null(cal.fname())) {
                cal$load_signal(cal.fname());
                cal$explore_numerically();
                cal$plot();
                if (!is.null(cal.model())) {
                    cal$fit_model(cal.model());
                    cal$plot_fit(cal.model());
                }
            }
        })  ## End of output$cal.Plot
        ## output$PlotTG <- renderPlot({ ## runs each time user changes a widget
        ##     inFile <- input$data.tg;
        ##     if (!is.null(inFile)) {
        ##         e$data.tg <- read.table(file = inFile$datapath, header = TRUE,
        ##                                 col.names = c("x", "y"), sep = " ");
        ##         PlotSignal(e, signal.type = "Thrombin generation");
        ##     }
        ## })  ## End of output$PlotTG
        ## output$PlotThromb <- renderPlot({ ## runs each time user changes a widget
        ##     if (!is.null(e$data.tg)) {
        ##         GetDrv1(e);
        ##         PlotSignal(e, signal.type = "Thrombogram");
        ##     }
        ## })  ## End of output$PlotThromb
        ## output$ParCalibr <- renderDataTable({
        ##     e$df.cal;
        ## })  ## End of output$ParCalibr
        cal.model <- reactive({
            switch(input$cal.model,
                   "LateMM" = "LateMM",
                   "LateExp" = "LateExp",
                   "EarlyMM" = "EarlyMM",
                   "LM" = "LM",
                   "Auto" = "Auto")
        })
        output$cal.model <- renderText({
            if (!is.null(cal.model()) && !is.null(cal.fname())) {
                paste0(cal.model(), " selected to fit calibration data ",
                       cal.fname()$name);
            } else {
                paste(">> Model not selected or data not loaded.");
            }
        })  ## End of output$cal.model
        ## output$PlotFit <- renderPlot({
        ##     PlotFit(e);
        ## })
    }  ## End of function
)  ## End of shinyServer
################################################################################

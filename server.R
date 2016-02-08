library(shiny); library(minpack.lm); library(LambertW);
print("################################################################################");
source("src/global_parameters.R");
source("src/Base_class.R");
source("src/Cal_class.R");
## source("src/tg_class.R");
## source("src/plotting_functions.R");
## source("src/numerical_derivatives.R");
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
        })  ## End of cal.fname
        cal.data <- reactive({
            if (!is.null(cal.fname())) {
                cal$clear();
                cal$load_signal(cal.fname());
                cal$explore_numerically();
            }
        })
        cal.model <- reactive({
            switch(input$cal.model,
                   "LateMM" = "LateMM",
                   "LateExp" = "LateExp",
                   "EarlyMM" = "EarlyMM",
                   "LM" = "LM",
                   "Auto" = "Auto")
        })  ## End of cal.model
        output$cal.model <- renderText({
            if (!is.null(cal.model()) && !is.null(cal.data())) {
                paste0(cal.model(), " selected to fit calibration data ",
                       cal.fname()$name);
            } else {
                paste(">> Model not selected or data not loaded.");
            }
        })  ## End of output$cal.model
        cal.e0 <- reactive ({
            cal$set_e0(input$cal.e0);
        })
        cal.s0 <- reactive ({
            cal$set_s0(input$cal.s0);
        })
        output$cal.Plot <- renderPlot({ ## runs each time user changes a widget
            if (!is.null(cal.data())) {
                ## cal$load_signal(cal.fname());
                ## cal$explore_numerically();
                cal$plot();
                if (!is.null(cal.model())) {
                    print(cal.model());
                    cal.e0(); cal.s0();
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
        output$cal.parms <- renderDataTable(expr = {
            if (!is.null(cal.model())) {
                cal.e0(); cal.s0();
                ## print(cal$e0); print(cal$s0);
                cal$parms_model(cal.model());
            }
        })  ## End of output$ParCalibr
    }  ## End of function
)  ## End of shinyServer
################################################################################

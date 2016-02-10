library(shiny); library(minpack.lm); library(LambertW);
print("################################################################################");
source("src/global_parameters.R");
source("src/common_functions.R");
source("src/Base_class.R");
source("src/Cal_class.R");
source("src/TG_class.R");
## source("src/plotting_functions.R");
## source("src/numerical_derivatives.R");
## code here runs once when app is launched
cal <- Cal$new(); tg <- TG$new();
################################################################################
shinyServer(
    func = function(input, output) { ## runs each time a user visits the app
######################################## Calibration signal tab
        cal.fname <- reactive(x = {
            return(input$cal.fname);
        })  ## End of cal.fname
        cal.data <- reactive(x = {
            if (!is.null(cal.fname())) {
                cal$clear();
                cal$load_signal(cal.fname());
                cal$explore_numerically();
                return(0);
            }
        })  ## End of cal.data
        cal.model <- reactive(x = {
            print(paste0(">> cal.model is ", input$cal.model));
            return(input$cal.model);
        })  ## End of cal.model
        output$cal.Plot <- renderPlot(expr = { ## runs each time user changes a widget
            if (!is.null(cal.data())) {  ## data is now loaded
                cal$plot();
                if (!is.null(cal.model())) {  ## model is now chosen
                    ## print(cal.model());
                    ## cal.e0(); cal.s0();
                    cal$fit_model(cal.model());
                    cal$plot_fit(cal.model());
                }
            }
        })  ## End of output$cal.Plot
        output$cal.model <- renderUI(expr = {
            if (!is.null(cal.model()) && !is.null(cal.data())) {
                paste0(cal.model(), " selected to fit calibration data ",
                       cal.fname()$name); ##exists(x = cal.model(), where = cal$fit)
                if (exists(x = cal.model(), where = cal$fit)) {
                ##     paste0(cal.model(), "fit also exists");
                x <- GetSummary(cal$fit[[cal.model()]]$smry);
                HTML(paste(x, collapse = '<br/>'));
                }
            } else {
                paste(">> Model not selected or data not loaded.");
            }
        })  ## End of output$cal.model
        cal.e0 <- reactive(x = {
            cal$set_e0(input$cal.e0);
        })
        cal.s0 <- reactive(x = {
            cal$set_s0(input$cal.s0);
        })
######################################## Thrombin generation signal tab
        tg.fname <- reactive(x = {
            return(input$tg.fname);
        })  ## End of tg.fname
        tg.data <- reactive(x = {
            if (!is.null(tg.fname())) {
                tg$clear();
                tg$load_signal(tg.fname());
                tg$explore_numerically();
                return(0);
            }
        })  ## End of tg.data
        output$tg.Plot <- renderPlot(expr = { ## runs each time user changes a widget
            if (!is.null(tg.data())) {  ## data is now loaded
                tg$plot();
            }
        })  ## End of output$tg.Plot
######################################## Thrombogram tab
        output$tg.PlotDrv1 <- renderPlot(expr = { ## runs each time user changes a widget 
            if (!is.null(tg.data()) && length(tg$num.smry) != 0) {
                tg$plot_drv1();
            }
        })  ## End of output$PlotThromb
######################################## Parameters tab
        output$cal.ShowParms <- renderDataTable(expr = {
            if (!is.null(cal.model()) && exists(x = cal.model(), where = cal$fit)) {
                cal.e0(); cal.s0();
                ## print(cal$e0); print(cal$s0);
                cal$parms_model(cal.model());
            } else {
                NULL;
            }
        })  ## End of output$cal.ShowParms
    }  ## End of function
)  ## End of shinyServer
################################################################################

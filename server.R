library(shiny); library(minpack.lm); library(LambertW);
print("################################################################################");
source("src/global_parameters.R");
source("src/common_functions.R");
source("src/Base_class.R");
source("src/Cal_class.R");
source("src/TG_class.R");
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
                ## cal.model(); cal.model.fit();  TODO fix fitting with chosen model when a new file is loaded
                return(0);
            }
        })  ## End of cal.data
        cal.model <- reactive(x = {
            print(paste0(">> cal.model is ", input$cal.model));
            return(input$cal.model);
        })  ## End of cal.model
        cal.model.fit <- reactive(x = {
            if (!is.null(cal.model()) && cal.model() != "None") {
                cal$fit_model(cal.model());
                return(0);
            } else {
                return(NULL);
            }
        })  ## End of cal.model.fit
        output$cal.Plot <- renderPlot(expr = { ## runs each time user changes a widget
            if (!is.null(cal.data())) {  ## data is now loaded
                cal$plot();
                if (!is.null(cal.model.fit()))
                    cal$plot_fit(cal.model());
            }
        })  ## End of output$cal.Plot
        output$cal.model <- renderUI(expr = {
            if (!is.null(cal.model()) && cal.model() != "None" && !is.null(cal.data())) {
                paste0(cal.model(), " selected to fit calibration data ",
                       cal.fname()$name);
                if (!is.null(cal.model.fit()) && exists(x = cal.model(), where = cal$fit)) {
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
        cal.CF <- reactive({
            return(input$cal.CF);
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
        tg.model <- reactive(x = {
            print(paste0(">> tg.model is ", input$tg.model));
            return(input$tg.model);
        })  ## End of tg.model
        tg.model.fit <- reactive(x = {
            if (!is.null(tg.model()) && tg.model() != "None") {
                tg$fit_model(tg.model());
                return(0);
            } else {
                return(NULL);
            }
        })  ## End of tg.model.fit
        output$tg.Plot <- renderPlot(expr = { ## runs each time user changes a widget
            if (!is.null(tg.data())) {  ## data is now loaded
                tg$plot();
                if (!is.null(tg.model.fit()))
                    tg$plot_fit(tg.model());
            }
        })  ## End of output$tg.Plot
        output$tg.model <- renderUI(expr = {
            if (!is.null(tg.model()) && tg.model() != "None" && !is.null(tg.data())) {
                paste0(tg.model(), " selected to fit thrombin generation data ",
                       tg.fname()$name);
                if (!is.null(tg.model.fit()) && exists(x = tg.model(), where = tg$fit)) {
                    x <- GetSummary(tg$fit[[tg.model()]]$smry);
                    HTML(paste(x, collapse = '<br/>'));
                }
            } else {
                paste(">> Model not selected or data not loaded.");
            }
        })  ## End of output$tg.model
######################################## Thrombogram tab
        output$tg.PlotDrv1 <- renderPlot(expr = { ## runs each time user changes a widget
                if (!is.null(tg.data()) && length(tg$num.smry) != 0) {
                    par(mfrow = c(1, 2));
                    tg$plot_drv1(); tg$plot_drv2();
                    if (!is.null(tg.model.fit())) {
                        par(mfrow = c(1, 2));
                        tg$plot_thrombogram(tg.model());
                        tg$plot_velocity(tg.model());
                    }
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
        output$tg.ShowParms <- renderDataTable(expr = {
            if (!is.null(tg.model()) && exists(x = tg.model(), where = tg$fit)) {
                tg$parms_model(tg.model(), cal.CF());
            } else {
                NULL;
            }
        })  ## End of output$tg.ShowParms
    }  ## End of function
)  ## End of shinyServer
################################################################################

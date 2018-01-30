################################################################################
################################################################################
######################################## Calibration
################################################################################
################################################################################

Cal <- reactiveValues(data_loaded = FALSE, model_fitted = FALSE);
observeEvent(  ## tracks input$cal.fname
    eventExpr = input$cal.fname, handlerExpr = {
        if (!is.null(input$cal.fname)) {
            if (Cal$data_loaded) {
                cal$clear();
                Cal$data_loaded <- FALSE;
            }
            if (Cal$model_fitted)
                Cal$model_fitted <- FALSE;
            cal$load_signal(input$cal.fname);
            cal$explore_numerically();
            Cal$data_loaded <- TRUE;
        }
    })  ## End of observeEvent input$cal.fname
observeEvent(  ## tracks input$cal.model
    eventExpr = input$cal.model, handlerExpr = {
        if (!is.null(input$cal.model) && input$cal.model != "None") {
            if (Cal$model_fitted)
                Cal$model_fitted <- FALSE;
            progress <- shiny::Progress$new();
            progress$set(message = paste0("Fitting ", input$cal.model,
                                          "..."), value = 0);
            on.exit(progress$close());
            cal$fit_model(cal.model = input$cal.model, cal$updateProgess,
                          weights = input$weights);
            if (cal$model_exists(input$cal.model))
                Cal$model_fitted <- TRUE;
        } else {
            Cal$model_fitted <- FALSE;
        }
    })  ## End of observeEvent input$cal.model
############################################################
######################################## Calibration sidebar
############################################################

output$cal.Menu <- renderUI({
    if (Cal$data_loaded) {
        return(
            selectInput(inputId = "cal.model",
                        label = h5("Select model to fit calibration signal"),
                        choices = c("Auto",
                                    "CombinedMM",
                                    "T0LateMM", "LateMM",
                                    "T0LateExp", "LateExp",
                                    "EarlyMM", "LM",
                                    "None"),
                        selected = "None")
        );
    }
})  ## End of output$cal.Menu
############################################################
######################################## Calibration signal tab
############################################################

output$cal.Plot <- renderPlot({
    if (Cal$data_loaded) {
        cal$plot();
        if (Cal$model_fitted)
            cal$plot_fit(input$cal.model);
    }
})  ## End of output$cal.Plot
output$cal.PlotResid <- renderPlot({
    if (Cal$model_fitted) {
        if (input$cal.model != "Auto") {
            cal$plot_residuals(input$cal.model);
        } else if (!cal$is_none_auto_model()) {
            cal$plot_residuals("Auto");
                }
    }
})  ## End of output$cal.PlotResid
output$cal.model <- renderUI({
    if (Cal$model_fitted) {
        HTML(c("<pre>",
               paste(cal$print_summary(cal$get_summary(input$cal.model)),
                     collapse = '<br/>'), "</pre>"));
    }
})  ## End of output$cal.model
## output$cal.SynthHint <- renderTable({
##     if (!is.null(cal.data()) && !is.null(cal.model()) &&
##         cal.model() != "None" &&
##         substr(input$cal.fname$name, 0, 9) == "Synthetic") {
##         switch(cal.model(),
##                "LM" = { return(t(data.frame(
##                    Parameter_Name = c("Intercept", "Slope", "Residual standard error"),
##                    True_Value = c(100, 10, 2))));
##                     },
##                "EarlyMM" = { return(t(data.frame(
##                    Parameter_Name = c("b", "p1", "p2", "Residual standard error"),
##                    True_Value = c(10, 10, 1, 2))));
##                          },
##                "LateExp" = { return(t(data.frame(
##                    Parameter_Name = c("b", "p1", "p3", "Residual standard error"),
##                    True_Value = c(10, 1000, 0.05, 2))));
##                          },
##                "LateMM" = { return(t(data.frame(
##                    Parameter_Name = c("b", "p1", "p2", "p3", "Residual standard error"),
##                    True_Value = c(10, 1500, 0.75, 0.1, 2))));
##                         },
##                { ## Default
##                    warning(">> Returning NULL table!");
##                    return(NULL);
##                }
##                );  ## End of switch()
##     }  ## End of if()
## })  ## End of output$cal.SynthHint

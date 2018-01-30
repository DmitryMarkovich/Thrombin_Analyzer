################################################################################
################################################################################
######################################## Thrombin generation
################################################################################
################################################################################

TG <- reactiveValues(data_loaded = FALSE, model_fitted = FALSE);
observeEvent(  ## tracks input$tg.fname
    eventExpr = input$tg.fname, handlerExpr = {
        if (!is.null(input$tg.fname)) {
            if (TG$data_loaded) {
                tg$clear();
                TG$data_loaded <- FALSE;
            }
            if (TG$model_fitted)
                TG$model_fitted <- FALSE;
            tg$load_signal(input$tg.fname);
            tg$explore_numerically();
            tg$evaluate_numerically();
            TG$data_loaded <- TRUE;
        }
    })  ## End of observeEvent input$tg.fname
observeEvent(  ## tracks input$dataset.add.to.tg
    eventExpr = input$dataset.add.to.tg, handlerExpr = {
        if (!is.null(input$dataset.add.to.tg) &&
            input$dataset.add.to.tg != "None") {
            if (TG$data_loaded) {
                tg$clear();
                TG$data_loaded <- FALSE;
            }
            if (TG$model_fitted)
                TG$model_fitted <- FALSE;
                    tg <<- dataset$copy_and_analyze_TG(
                        x = dataset$get_time(),
                        y = dataset$get_data_column(input$dataset.add.to.tg),
                        signal = input$dataset.add.to.tg, copy.res = FALSE);
            tg$explore_numerically();
            tg$evaluate_numerically();
            TG$data_loaded <- TRUE;
        }
    })  ## End of observeEvent input$dataset.add.to.tg
observeEvent(  ## tracks input$tg.model
    eventExpr = input$tg.model, handlerExpr = {
        if (!is.null(input$tg.model) && input$tg.model != "None") {
            if (TG$model_fitted)
                TG$model_fitted <- FALSE;
            progress <- shiny::Progress$new();
            progress$set(message = paste0("Fitting ", input$tg.model,
                                          "..."), value = 0);
            on.exit(progress$close());
            tg$fit_model(input$tg.model, tg$updateProgress, progress,
                         silent = TRUE, weights = input$weights);
            if (tg$model_exists(input$tg.model))
                TG$model_fitted <- TRUE;
        } else {
            TG$model_fitted <- FALSE;
        }
    })  ## End of observeEvent input$tg.model
############################################################
######################################## Thrombin generation sidebar
############################################################

output$tg.Menu <- renderUI({
            if (TG$data_loaded) {
                return(
                    column(width = 12,
                           selectInput(inputId = "tg.model",
                                       label = h5("Select model to fit thrombin generation signal"), 
                                       choices = c("Auto",
                                                   "LateExpT0GammaInt2",
                                                   "LateExpT0GammaInt",
                                                   ## "LateExpGammaInt",
                                                   ## "T0GammaInt2_test",
                                                   "T0GammaInt2",
                                                   "T0GammaInt",
                                                   ## "GammaInt",
                                                   "T0Gamma",
                                                   "Pade",
                                                   ## "Gamma",
                                                   "None"),
                                       selected = "None")
                           )
                );
            }
})  ## End of output$tg.Menu
############################################################
######################################## Thrombin generation signal tab
############################################################

output$tg.Plot <- renderPlot({
    if (TG$data_loaded) {
        tg$plot();
        if (TG$model_fitted) {
            tg$plot_fit(input$tg.model);
                }
    }
})  ## End of output$tg.Plot
output$tg.PlotResid <- renderPlot({
    if (TG$model_fitted) {
        if (input$tg.model != "Auto") {
            tg$plot_residuals(input$tg.model);
        } else if (!tg$is_none_auto_model()) {
            tg$plot_residuals("Auto");
        }
    }
})  ## End of output$tg.PlotResid
output$tg.model <- renderUI({
    if (TG$model_fitted) {
        if (input$tg.model != "Auto") {
            HTML(c("<pre>",
                   paste(tg$print_summary(tg$get_summary(input$tg.model)),
                                 collapse = '<br/>'), "</pre>"));
        } else if (!tg$is_none_auto_model()) {
            HTML(c("<pre>",
                   paste(tg$print_summary(tg$get_summary("Auto")),
                         collapse = '<br/>'), "</pre>"));
        }
    }
})  ## End of output$tg.model
## output$tg.SynthHint <- renderTable({
##     if (!is.null(tg.model.fit()) && tg.model() != "None" &&
##         substr(input$tg.fname$name, 0, 9) == "Synthetic") {
##         switch(tg.model(),
##                "Gamma" = { return(t(data.frame(
##                    Parameter_Name = c("b", "A", "k", "theta", "Residual standard error"),
##                    True_Value = c(50, 500, 3, 7, 2))));
##                        },
##                "T0Gamma" = { return(t(data.frame(
##                    Parameter_Name = c("b", "A", "k", "theta", "t0", "Residual standard error"),
##                    True_Value = c(50, 500, 3, 7, 10, 2))));
##                          },
##                "GammaInt" = { return(t(data.frame(
##                    Parameter_Name = c("b", "A", "k", "theta", "k.a2m", "Residual standard error"),
##                    True_Value = c(10, 600, 5, 2, 0.01, 2))));
##                           },
##                "T0GammaInt" = { return(t(data.frame(
##                    Parameter_Name = c("b", "A", "k", "theta", "k.a2m", "t0", "Residual standard error"),
##                    True_Value = c(10, 600, 5, 2, 0.01, 10, 2))));
##                             },
##                "LateExpGammaInt" = { return(t(data.frame(
##                    Parameter_Name = c("b", "p1", "A", "k", "theta", "k.a2m", "Residual standard error"),
##                    True_Value = c(10, 1200, 0.5, 5, 2, 0.01, 2))));
##                                  },
##                "LateExpT0GammaInt" = { return(t(data.frame(
##                    Parameter_Name = c("b", "p1", "A", "k", "theta", "k.a2m", "t0", "Residual standard error"),
##                    True_Value = c(10, 1200, 0.5, 5, 2, 0.01, 10, 2))));
##                                    },
##                { ## Default
##                    warning(">> Returning NULL table!");
##                    return(NULL);
##                }
##                );  ## End of switch()
##     }  ## End of if()
## })  ## End of output$tg.SynthHint
############################################################
######################################## Thrombogram tab
############################################################

output$tg.PlotDrv1 <- renderPlot({
    if (TG$data_loaded && tg$is_ok_num_smry()) {
        tg$plot_drv1();
        if (TG$model_fitted) {
            tg$plot_thrombogram(input$tg.model);
        }
    }
})  ## End of output$tg.PlotDrv1
output$tg.PlotDrv2 <- renderPlot({
    if (TG$data_loaded && tg$is_ok_num_smry()) {
        tg$plot_drv2();
        if (TG$model_fitted) {
            tg$plot_velocity(input$tg.model);
        }
    }
})  ## End of output$tg.PlotDrv2
output$tg.PlotPeriodogram <- renderPlot({
    if (TG$data_loaded && tg$is_ok_num_smry()) {
        tg$plot_periodogram();
    }
})  ## End of output$PlotPeriodogram

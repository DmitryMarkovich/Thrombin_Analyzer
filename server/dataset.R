################################################################################
################################################################################
######################################## Dataset
################################################################################
################################################################################

Dataset <- reactiveValues(
    DO_Analysis = FALSE, res = NULL,
    data_loaded = FALSE, res_loaded = FALSE, parms_loaded = FALSE
);
############################################################
######################################## Dataset sidebar
############################################################

observeEvent(  ## tracks input$dataset.fname
    eventExpr = input$dataset.fname, handlerExpr = {
        if (!is.null(input$dataset.fname)) {
            if (Dataset$DO_Analysis || (Dataset$data_loaded)) {
                ## load a new dataset when the analysis has been done
                dataset$clear();
                Dataset$DO_Analysis <- FALSE;
                Dataset$data_loaded <- FALSE;
                Dataset$res_loaded <- FALSE;
                Dataset$parms_loaded <- FALSE;
                ## USER$Selection <- NULL;
            }
            dataset$load(input$dataset.fname);
            Dataset$data_loaded <- TRUE;
        }
    })  ## End of observeEvent input$dataset.fname

observeEvent(  ## tracks input$res.fname
    eventExpr = input$res.fname, handlerExpr = {
        if (!is.null(input$res.fname)) {
            if (Dataset$res_loaded)
                Dataset$res_loaded <- FALSE;
            dataset$load_results(input$res.fname);
            Dataset$res_loaded <- TRUE;
        }
    })  ## End of observeEvent input$res.fname

observeEvent(  ## tracks input$parms.fname
    eventExpr = input$parms.fname, handlerExpr = {
        if (!is.null(input$parms.fname)) {
            if (Dataset$parms_loaded)
                Dataset$parms_loaded <- FALSE;
            dataset$load_parms(input$parms.fname);
            Dataset$parms_loaded <- TRUE;
        }
    })  ## End of observeEvent input$parms.fname

output$dataset.Menu <- renderUI({
    if (Dataset$data_loaded) {
        return(list(
            fluidRow(
                column(width = 3, offset = 0,
                       actionButton(inputId = "dataset.analyze",
                                    label = "Analyze!",
                                    inline = TRUE)
                       ),
                column(width = 9, offset = 0, align = "center",
                       radioButtons(inputId = "dataset.show",
                                    label = "Show as",
                                    choices = list("plot" = "plot",
                                                   "text" = "text"),
                                    selected = "plot", inline = TRUE)
                       )
            ),
            fluidRow(
                column(width = 12, offset = 0, align = "center",
                       uiOutput(outputId = "parms.Download"),
                       uiOutput(outputId = "res.Download")
                       )
            ),  ## End of FluidRow
            fluidRow(
                column(width = 6, offset = -1,
                       selectInput(inputId = "dataset.overlay1",
                                   label = h5("Overlay"),
                                   choices = as.list(c(dataset$get_signals()[-1], "None")),
                                   selected = "None")
                       ),
                column(width = 6, offset = -1,
                       selectInput(inputId = "dataset.overlay2",
                                   label = h5("with"),
                                   choices = as.list(c(dataset$get_signals()[-1], "None")),
                                   selected = "None")
                       )
            ),  ## End of fluidRow
            h3("Add to", align = "center"),
            fluidRow(
                column(width = 6, offset = -1,
                       selectInput(inputId = "dataset.add.to.cal",
                                   label = h6("Calibration"),
                                   choices = as.list(c(dataset$get_signals()[-1], "None")),
                                   selected = "None")
                       ),
                column(width = 6, offset = -1,
                       selectInput(inputId = "dataset.add.to.tg",
                                   label = h6("Thrombin generation"),
                                   choices = as.list(c(dataset$get_signals()[-1], "None")),
                                   selected = "None")
                       )
                )
        ));
    }  ## End of if ()
})  ## End of output$dataset.Menu
########################################
#################### Downloads
########################################

output$parms.Download <- renderUI({
    if (Dataset$data_loaded &&
        Dataset$DO_Analysis) {
        downloadButton(outputId = "parms.Download_active",
                       "Download parameters");
    }
})  ## End of output$parms.Download

output$parms.Download_active <- downloadHandler(
    filename = function() paste0(Sys.Date(), "-",
                                 format(Sys.time(), "%H-%M-%S"),
                                 "-parameters-", input$dataset.fname),
    content = function(file) {
        write.table(x = dataset$get_parms(), file = file, quote = FALSE,
                    sep = ";", row.names = FALSE);
    }
)  ## End of output$parms.Download_active

output$res.Download <- renderUI({
    if (Dataset$data_loaded &&
        Dataset$DO_Analysis) {
        downloadButton(outputId = "res.Download_active",
                       "Download results");
        ## str(dataset$res);
    }
})  ## End of output$res.Download

## Create a reactive value rv to export Dataset$res
rv <- reactiveValues();
observe({
    if (Dataset$DO_Analysis && length(Dataset$res) != 0) {
        isolate(rv <<- Dataset$res)
    }
})  ## End of observe rv

output$res.Download_active <- downloadHandler(
    filename = function() paste0(Sys.Date(), "-",
                                 format(Sys.time(), "%H-%M-%S"), "-results-",
                                 substr(input$dataset.fname, 0,
                                        nchar(input$dataset.fname) - 4),
                                 ".RData"),
    content = function(file) {
        save(rv, file = file);
    }
)  ## End of output$res.Download_active
########################################
#################### Overlays
########################################

observeEvent(  ## tracks input$dataset.overlay1
    eventExpr = input$dataset.overlay1, handlerExpr = {
        if (!is.null(input$dataset.overlay1) &&
            input$dataset.overlay1 != "None") {
            dataset$set_tg(input$dataset.overlay1, 1);
        }
    })  ## End of observeEvent input$dataset.overlay1

observeEvent(  ## tracks input$dataset.overlay2
    eventExpr = input$dataset.overlay2, handlerExpr = {
        if (!is.null(input$dataset.overlay2) &&
            input$dataset.overlay2 != "None") {
            dataset$set_tg(input$dataset.overlay2, 2);
        }
    })  ## End of observeEvent input$dataset.overlay1

observeEvent(  ## tracks Dataset$DO_Analysis to update tg1 and tg2
    eventExpr = Dataset$DO_Analysis, handlerExpr = {
        if (!is.null(Dataset$DO_Analysis) &&
            Dataset$DO_Analysis) {
            if (!is.null(input$dataset.overlay1) &&
                input$dataset.overlay1 != "None") {
                dataset$set_tg(input$dataset.overlay1, 1);
            }
            if (!is.null(input$dataset.overlay2) &&
                input$dataset.overlay2 != "None") {
                dataset$set_tg(input$dataset.overlay2, 2);
            }
        }
    })  ## End of observeEvent Dataset$DO_Analysis
############################################################
######################################## Dataset tab
############################################################

dataset.show <- reactive({
    if (Dataset$data_loaded) {
        return(input$dataset.show);
    } else {
        return(NULL);
    }
})  ## End of dataset.show

output$dataset.ShowAs <- renderUI({
    if (!is.null(dataset.show())) {
        switch(dataset.show(),
               "plot" = return(
                   tagList(renderPlot({
                       progress <- shiny::Progress$new();
                       progress$set(message = "Plotting dataset", value = 0);
                       on.exit(progress$close());
                       dataset$plot(dataset$updateProgress, progress);
                   })  ## End of RenderPlot
                   )  ## End of tagList
               ),
               "text" = return(
                   tagList(renderTable({
                       progress <- shiny::Progress$new();
                       progress$set(message = "Showing dataset as text, please wait...", value = 0);
                       on.exit(progress$close());
                       dataset$get_data();
                   }, digits = 6)  ## End of renderTable
                   )  ## End of tagList
               )
               );  ## End of switch
    } else {
        return(NULL);
    }
})  ## End of output$dataset.ShowAs

observeEvent(  ## tracks input$dataset.analyze
    eventExpr = input$dataset.analyze, handlerExpr = {
        if (!is.null(input$dataset.analyze)) {
            Dataset$DO_Analysis <- TRUE;
            progress <- shiny::Progress$new();
            progress$set(message = "Analyzing dataset,", value = 0);
            on.exit(progress$close());
            ## does the auto analysis
            dataset$do_analysis(dataset$updateProgress, progress);
            Dataset$res <- dataset$get_res();
        }
    })  ## End of observeEvent input$dataset.analyze

output$dataset.ShowLagtime <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "Lagtime");
    }
})  ## End of output$dataset.ShowLagtime

output$dataset.ShowttPeak <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "ttPeak");
    }
})  ## End of output$dataset.ShowttPeak

output$dataset.ShowETP <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "ETP");
    }
})  ## End of output$dataset.ShowETP

observeEvent(  ## tracks input$plot_brush
    eventExpr = input$plot_brush, handlerExpr = {
        if (USER$Logout) {
            ## remove selection when logout
            USER$Selection <- NULL;
        } else {
            USER$Selection <- input$plot_brush;
        }
    })  ## End of observeEvent input$plot_brush

output$brush_info <- renderPrint({
    if ((Dataset$data_loaded && Dataset$DO_Analysis &&
         !is.null(USER$Selection)) || Dataset$parms_loaded) {
                brushedPoints(dataset$get_parms(na.rm = TRUE), USER$Selection,
                              xvar = "Num", yvar = "ETP");
            }
        })  ## End of output$brush_info , width = 00

output$dataset.ShowVelIndex <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "VelIndex");
    }
})  ## End of output$dataset.ShowVelIndex

output$dataset.ShowPeak <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "Peak");
    }
})  ## End of output$dataset.ShowPeak

output$dataset.ShowAlpha2M_Level <- renderPlot({
    if ((Dataset$data_loaded && Dataset$DO_Analysis) ||
        Dataset$parms_loaded) {
        dataset$visualize_parameters(which = "Alpha2M_Level");
    }
})  ## End of output$dataset.ShowAlpha2M_Level
############################################################
######################################## Overlay tab
############################################################

        output$dataset.PlotOverlay <- renderPlot({
            if (Dataset$data_loaded) {
                Dataset$DO_Analysis;  ## to take dependency on Analyze button
                if (!is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None") {
                    dataset$plot_overlay(input$dataset.overlay1,
                                         input$dataset.overlay2);
                }
            } else {
                return(NULL);
            }
        })  ## End of output$dataset.PlotOverlay
        output$dataset.PlotDrvOverlay <- renderPlot({
            if (Dataset$data_loaded) {
                par(mfrow = c(1, 2));
                if ((Dataset$res_loaded || (Dataset$DO_Analysis)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None" &&
                    dataset$res_length_ok()) {
                    dataset$plot_drv1_overlay(input$dataset.overlay1,
                                              input$dataset.overlay2);
                    dataset$plot_drv2_overlay(input$dataset.overlay1,
                                              input$dataset.overlay2);
                } else {
                    return(NULL);
                }
            }
        })  ## End of output$dataset.PlotDrvOverlay
        output$dataset.ShowParmsOverlay <- renderTable({
            if (Dataset$data_loaded) {
                if ((Dataset$res_loaded ||
                         (!is.null(Dataset$DO_Analysis) &&
                              Dataset$DO_Analysis)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None" &&
                    dataset$res_length_ok()) {
                    return(dataset$overlay_parms(input$dataset.overlay1,
                                                input$dataset.overlay2));
                } else {
                    return(NULL);
                }
            }
        }, digits = 3)  ## End of output$dataset.ShowParmsOverlay
############################################################
######################################## Overlay details tab
############################################################

        output$dataset.PlotResidOverlay <- renderPlot({
            if (Dataset$data_loaded) {
                if ((Dataset$res_loaded ||
                         (!is.null(Dataset$DO_Analysis) &&
                              Dataset$DO_Analysis)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None") {
                    layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE),
                           width = c(1, 1), heights = c(1, 1));
                    dataset$plot_overlay(input$dataset.overlay1,
                                         input$dataset.overlay2);
                    dataset$plot_resid_overlay(input$dataset.overlay1,
                                               input$dataset.overlay2);
                }
            } else {
                return(NULL);
            }
        })  ## End of output$dataset.PlotResidOverlay
        output$dataset.ShowSmryOverlay <- renderUI({
            if (Dataset$data_loaded) {
                if ((Dataset$res_loaded ||
                         (!is.null(Dataset$DO_Analysis) &&
                              Dataset$DO_Analysis)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None") {
                    fluidRow(
                            column(width = 6,
                                   h4(input$dataset.overlay1, align = "center"),
                                   HTML(c("<pre>", paste(
                                       dataset$get_tg(1)$print_summary(
                                           dataset$get_tg(1)$get_summary("Auto")),
                                       collapse = '<br/>'), "</pre>"))
                                   ),
                        column(width = 6,
                                   h4(input$dataset.overlay2, align = "center"),
                               HTML(c("<pre>", paste(
                                   dataset$get_tg(2)$print_summary(
                                       dataset$get_tg(2)$get_summary("Auto")),
                                   collapse = '<br/>'), "</pre>"))
                               )
                        )  ## End of fluidRow
                }
            } else {
                return(NULL);
            }
        })  ## End of output$dataset.ShowSmryOverlay

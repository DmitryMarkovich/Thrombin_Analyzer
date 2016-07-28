print("######################################## >> Thrombin Analyzer reloaded! << ########################################");
source("src/libraries.R");
app.path <- getwd();  ## stores current directory
source("src/global_parameters.R");
source("src/Base_class.R"); source("src/Cal_class.R"); source("src/TG_class.R");
source("src/Dataset_class.R");
dataset <- Dataset$new(); cal <- Cal$new(); tg <- TG$new();
print(">> Ready to analyze!");
################################################################################
shinyServer(
    func = function(input, output, session) {
################################################################################
################################################################################
######################################## Login
################################################################################
################################################################################

        USER <- reactiveValues(Logged = FALSE, Login = FALSE, Logout = FALSE,
                               UsernameError = FALSE, PasswordError = FALSE,
                               Selection = NULL);
        observe({  ## shows the login page login_ui() or app page app_ui()
            if (USER$Logged == FALSE) {
                output$page <- renderUI({
                    div(class = "outer",
                        do.call(bootstrapPage, c("", login_ui())))
                })
            }
            if (USER$Logged == TRUE) {
                output$page <- renderUI({
                    app_ui();
                })
            }
        })  ## End of observe login_ui() or app_ui()
        observeEvent(  ## tracks input$Login
            eventExpr = input$Login, handlerExpr = {
                if (!USER$Login)
                    USER$Login <- TRUE;
            })  ## End of observeEvent input$Login
        output$UsernameError <- renderUI({
            if (USER$UsernameError) {
                HTML(">> Username is incorrect!");
            }
        })  ## End of output$UsernameError
        output$PasswordError <- renderUI({
            if (USER$PasswordError) {
                HTML(">> Password is incorrect!");
            }
        })  ## End of output$PasswordError
        observe({  ## logs the user in
            if (USER$Logged == FALSE) {
                if (USER$Login) {
                    if (any(input$Username == users$Username)) {
                        USER$UsernameError <- FALSE;
                        if (users$Password[users$Username == input$Username] ==
                            input$Password) {
                            USER$PasswordError <- FALSE;
                            USER$Logged <- TRUE; USER$Logout <- FALSE;
                            print(paste0(">> Logged with Username ",
                                         input$Username, " and Password ",
                                         input$Password));
                        } else {
                            USER$Login <- FALSE;
                            USER$PasswordError <- TRUE;
                        }
                    } else {
                        USER$Login <- FALSE;
                        USER$UsernameError <- TRUE;
                    }
                }
            }
        })  ## End of observe logs user in
        output$PrintUser <- renderUI({
            HTML(paste0("Hello ", input$Username, "!"));
        })  ## End of output$PrintUser
        observeEvent(  ## tracks input$Logout
            eventExpr = input$Logout, handlerExpr = {
                if (!USER$Logout)
                    USER$Logout <- TRUE;
            })  ## End of observeEvent input$Logout
        observe({  ## logs the user out
            if (USER$Logged == TRUE) {
                if (USER$Logout) {
                    print(">> Logout, clearing all!");
                    USER$Logged <- FALSE; USER$Login <- FALSE;
                    USER$Selection <- NULL;
                    Dataset$DO_Analysis <- FALSE; Dataset$res <- NULL;
                    Dataset$data_loaded <- FALSE; Dataset$res_loaded <- FALSE;
                    Dataset$parms_loaded <- FALSE;
                    dataset$clear();
                    Cal$data_loaded <- FALSE; Cal$model_fitted <- FALSE;
                    cal$clear();
                    TG$data_loaded <- FALSE; TG$model_fitted <- FALSE;
                    tg$clear();
                }
            }
        })  ## End of observe logs user out
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
                                            label = h5("Analyze!"),
                                            inline = TRUE)
                               ),
                        column(width = 3, offset = 0,
                               radioButtons(inputId = "dataset.show",
                                            label = h5("Show as"),
                                            choices = list("plot" = "plot",
                                                "text" = "text"),
                                            selected = "plot", inline = FALSE)
                               ),
                        column(width = 6, offset = 0,
                               uiOutput(outputId = "parms.Download"),
                               uiOutput(outputId = "res.Download")
                               )
                        ),  ## End of FluidRow
                    fluidRow(
                        column(width = 6, offset = -1,
                               selectInput(inputId = "dataset.overlay1",
                                           label = h5("Overlay"),
                                           choices = as.list(c(dataset$get_signals()[-1], "None")),
                                           selected = "None"),
                               selectInput(inputId = "dataset.add.to.cal",
                                           label = h6("Add to calibration"),
                                           choices = as.list(c(dataset$get_signals()[-1], "None")),
                                           selected = "None")
                               ),
                        column(width = 6, offset = -1,
                               selectInput(inputId = "dataset.overlay2",
                                           label = h5("with"),
                                           choices = as.list(c(dataset$get_signals()[-1], "None")),
                                           selected = "None"),
                               selectInput(inputId = "dataset.add.to.tg",
                                           label = h6("Add to thrombin generation"),
                                           choices = as.list(c(dataset$get_signals()[-1], "None")),
                                           selected = "None")
                               )
                        )  ## End of fluidRow
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
                format(Sys.time(), "%H-%M-%S"), "-parameters-", input$dataset.fname),
            content = function(file) {
                write.table(x = dataset$get_parms(), file = file, quote = FALSE, sep = ";");
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
                substr(input$dataset.fname, 0, nchar(input$dataset.fname) - 4),
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
        }, width = 100)  ## End of output$brush_in
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
                    cal$fit_model(input$cal.model);
                    if (cal$model_exists(input$cal.model))
                        Cal$model_fitted <- TRUE;
                }
            })  ## End of observeEvent input$cal.model
############################################################
######################################## Calibration sidebar
############################################################

        output$cal.Menu <- renderUI({
            if (Cal$data_loaded) {
                return(
                    selectInput(inputId = "cal.model",
                                label = h4("Select model to fit calibration signal"), 
                                choices = c("Auto", "T0LateMM", "LateMM",
                                    "T0LateExp", "LateExp", "EarlyMM",
                                    "LM", "None"),
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
                    tg$fit_model(input$tg.model);
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
                    column(width = 7,
                           selectInput(inputId = "tg.model",
                                       label = h4("Select model to fit thrombin generation signal"), 
                                       choices = c("Auto",
                                           "LateExpT0GammaInt",
                                           ## "LateExpGammaInt",
                                           "T0GammaInt2",
                                           "T0GammaInt",
                                           ## "GammaInt",
                                           "T0Gamma",
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
        })  ## End of output$PlotThromb
        output$tg.PlotDrv2 <- renderPlot({
            if (TG$data_loaded && tg$is_ok_num_smry()) {
                tg$plot_drv2();
                if (TG$model_fitted) {
                    tg$plot_velocity(input$tg.model);
                }
            }
        })  ## End of output$PlotThromb
################################################################################
################################################################################
######################################## Parameters tab
################################################################################
################################################################################

        output$cal.ShowParms <- renderTable({
            if (Cal$model_fitted) {
                cal$parms_model(input$cal.model, input$cal.e0, input$cal.s0);
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$cal.ShowParms
        output$tg.ShowParms <- renderTable({
            if (TG$model_fitted) {
                tg$parms_model(input$tg.model, input$cal.CF);
            }
        }, digits = 3)  ## End of output$tg.ShowParms
        output$tg.ShowParmsNum <- renderTable({
            if (TG$data_loaded) {
                return(tg$num_parms());
            }
        }, digits = 3)  ## End of output$tg.ShowParmsNum
################################################################################
################################################################################
######################################## Demo signals tab
################################################################################
################################################################################

        demo.signal <- reactive({
            signal <- input$demo.signal;
            switch(signal,
                   "Calibration - LM, EarlyMM" = {
                       return(list(
                           fname = "DEMO-Calibration-LM-EarlyMM.dat",
                           data = read.table(
                               file = "data/DEMO-Calibration-LM-EarlyMM.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Calibration - LateExp" = {
                       return(list(
                           fname = "DEMO-Calibration-LateExp.dat",
                           data = read.table(
                               file = "data/DEMO-Calibration-LateExp.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Calibration - LateMM" = {
                       return(list(
                           fname = "DEMO-Calibration-LateMM.dat",
                           data = read.table(
                               file = "data/DEMO-Calibration-LateMM.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Calibration - Paper" = {
                       return(list(
                           fname = "DEMO-Calibration-Paper.dat",
                           data = read.table(
                               file = "data/DEMO-Calibration-Paper.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - Gamma, T0Gamma" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-Gamma-T0Gamma.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-Gamma-T0Gamma.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - GammaInt, T0GammaInt" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-GammaInt-T0GammaInt.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-GammaInt-T0GammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - LateExpGammaInt, LateExpT0GammaInt" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-LateExpGammaInt-LateExpT0GammaInt.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-LateExpGammaInt-LateExpT0GammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - Paper Control" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-Paper-Control.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-Paper-Control.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - Paper Green" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-Paper-Green.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-Paper-Green.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Thrombin generation - Paper Red" = {
                       return(list(
                           fname = "DEMO-Thrombin-generation-Paper-Red.dat",
                           data = read.table(
                               file = "data/DEMO-Thrombin-generation-Paper-Red.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Calibration - LM" = {
                       return(list(
                           fname = "Synthetic-Calibration-LM.dat",
                           data = read.table(
                               file = "data/Synthetic-Calibration-LM.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Calibration - EarlyMM" = {
                       return(list(
                           fname = "Synthetic-Calibration-EarlyMM.dat",
                           data = read.table(
                               file = "data/Synthetic-Calibration-EarlyMM.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Calibration - LateExp" = {
                       return(list(
                           fname = "Synthetic-Calibration-LateExp.dat",
                           data = read.table(
                               file = "data/Synthetic-Calibration-LateExp.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Calibration - LateMM" = {
                       return(list(
                           fname = "Synthetic-Calibration-LateMM.dat",
                           data = read.table(
                               file = "data/Synthetic-Calibration-LateMM.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - Gamma" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-Gamma.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-Gamma.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - T0Gamma" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-T0Gamma.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-T0Gamma.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - GammaInt" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-GammaInt.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-GammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - T0GammaInt" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-T0GammaInt.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-T0GammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - LateExpGammaInt" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-LateExpGammaInt.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-LateExpGammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   "Synthetic - Thrombin generation - LateExpT0GammaInt" = {
                       return(list(
                           fname = "Synthetic-Thrombin-generation-LateExpT0GammaInt.dat",
                           data = read.table(
                               file = "data/Synthetic-Thrombin-generation-LateExpT0GammaInt.dat",
                               header = T, col.names = c("x", "y"), sep = " ")
                           ))
                   },
                   {  ## Default
                       ## warning(">> Unknown signal chosen!");
                       return(NULL);
                   }
                   )  ## End of switch
        })  ## End of demo.signal
        output$demo.Plot <- renderPlot({
            if (!is.null(demo.signal()) && demo.signal() != "None") {
                par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(demo.signal()$data, axes = FALSE, xlab = NA, ylab = NA,
                               cex = 1.25, lwd = 2,
                               ylim = c(0, max(demo.signal()$data$y))
                               );
                grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                box();
                axis(side = 1, tck = -0.025, labels = NA);
                axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                axis(side = 2, tck = -0.025, labels = NA, col = "black");
                axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                title(ylab = "Fluorescence, a.u.", line = 5.5, cex.lab = 1.5);
                title(main = input$demo.signal, line = 0.5, cex.main = 1.5);
            }
        })  ## End of output$demo.Plot
        output$demo.Download <- renderUI({
            if (!is.null(demo.signal()) && demo.signal() != "None") {
                downloadButton(outputId = "demo.Download_active",
                               "Download signal now!");
            }
        })  ## End of output$demo.Download
        output$demo.Download_active <- downloadHandler(
            filename = function() demo.signal()$fname,
            content = function(file) {
                write.table(x = demo.signal()$data, file = file)
            }
            )  ## End of output$demo.Download_active
################################################################################
################################################################################
######################################## Tutorial
################################################################################
################################################################################

        output$show.Tutorial <- renderText({
            return(paste('<iframe style="height:900px; width:100%" src="',
                         ## file has to be in ./www/ directory
                         paste0("Thrombin_Analyzer_tutorial.pdf"),
                         '"></iframe>', sep = ""));
        })  ## End of output$show.Tutorial
    }  ## End of function
)  ## End of shinyServer
################################################################################

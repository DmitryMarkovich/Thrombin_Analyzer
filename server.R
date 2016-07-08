print("######################################## >> Thrombin Analyzer reloaded! << ########################################");
source("src/libraries.R");
app.path <- getwd();  ## stores current directory
source("src/global_parameters.R");  ## source("src/common_functions.R");
source("src/Base_class.R"); source("src/Cal_class.R"); source("src/TG_class.R");
source("src/Dataset_class.R");

dataset <- Dataset$new(); cal <- Cal$new(); tg <- TG$new();
print(">> Ready to analyze!");
################################################################################
shinyServer(
    func = function(input, output) {
        USER <- reactiveValues(Logged = FALSE, Login = FALSE, Logout = FALSE,
                               UsernameError = FALSE, PasswordError = FALSE,
                               Selection = NULL);
################################################################################
######################################## Dataset tab
################################################################################
        Dataset <- reactiveValues(
            DO_Analysis = FALSE, res = NULL,
            data_loaded = FALSE, res_loaded = FALSE, parms_loaded = FALSE
            );
        ## dataset.fname <- reactive({
        ##     return(input$dataset.fname);
        ## })  ## End of dataset.fname
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
                    }
                    dataset$load(input$dataset.fname);
                    Dataset$data_loaded <- TRUE;
                }
            })  ## End of observeEvent input$dataset.fname
        ## dataset.data <- reactive({
        ##     if (!is.null(dataset.fname())) {
        ##         if (Dataset$DO_Analysis) {  ## !dataset$is_empty()
        ##             dataset$clear(); Dataset$DO_Analysis <- FALSE;
        ##         }
        ##         dataset$load(dataset.fname());
        ##         return(0L);
        ##     } else {
        ##         return(NULL);
        ##     }
        ## })  ## End of dataset.data
        ## res.fname <- reactive({
        ##     return(input$res.fname);
        ## })  ## End of res.fname
        ## res.data <- reactive({
        ##     if (!is.null(res.fname())) {
        ##         dataset$load_results(res.fname());
        ##         return(0L);
        ##     } else {
        ##         return(NULL);
        ##     }
        ## })  ## End of res.data
        observeEvent(  ## tracks input$res.fname
            eventExpr = input$res.fname, handlerExpr = {
                if (!is.null(input$res.fname)) {
                    if (Dataset$res_loaded)
                        Dataset$res_loaded <- FALSE;
                    dataset$load_results(input$res.fname);
                    Dataset$res_loaded <- TRUE;
                }
            })  ## End of observeEvent input$res.fname
        ## parms.fname <- reactive({
        ##     return(input$parms.fname);
        ## })  ## End of parms.fname
        ## parms.data <- reactive({
        ##     if (!is.null(parms.fname())) {
        ##         dataset$load_parms(parms.fname());
        ##         return(0L);
        ##     } else {
        ##         return(NULL);
        ##     }
        ## })  ## End of parms.data
        observeEvent(  ## tracks input$res.fname
            eventExpr = input$parms.fname, handlerExpr = {
                if (!is.null(input$parms.fname)) {
                    if (Dataset$parms_loaded)
                        Dataset$parms_loaded <- FALSE;
                    dataset$load_parms(input$parms.fname);
                    Dataset$parms_loaded <- TRUE;
                }
            })  ## End of observeEvent input$parms.fname
        ## output$dataset.ShowLoadedResults <- renderUI({
        ##     if (!is.null(res.data())) {
        ##         ## print(length(dataset$res));
        ##         ## print(paste0(">> Loaded ", length(dataset$res), " results"));
        ##         HTML(c("<pre>", paste0(">> Loaded ",
        ##                                dataset$get_res_length(), " results"), "</pre>"));
        ##     }
        ## })  ## End of output$dataset.ShowLoadedResults
        output$dataset.Menu <- renderUI({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
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
        dataset.show <- reactive({
            ## print(input$dataset.show);
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
                           ## print(">> renderPlot called from switch!");
                           progress <- shiny::Progress$new();  ## Create a Progress object
                           progress$set(message = "Plotting dataset", value = 0);
                           ## Close the progress when this reactive exits (even if there's an error)
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
                if (!is.null(input$dataset.analyze)) {  ## dataset$is_empty()
                    Dataset$DO_Analysis <- TRUE;
                    progress <- shiny::Progress$new();
                    progress$set(message = "Analyzing dataset,", value = 0);
                    on.exit(progress$close());
                    ## does the auto analysis
                    dataset$do_analysis(dataset$updateProgress, progress);
                    Dataset$res <- dataset$get_res();
                }
                ## else {
                ##     Dataset$DO_Analysis <- FALSE;
                ## }
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
                    USER$Selection <- NULL;
                } else {
                    USER$Selection <- input$plot_brush;
                }
                ## print(USER$Selection);
            })  ## End of observeEvent input$plot_brush
        output$brush_info <- renderPrint({
            if ((Dataset$data_loaded &&   ## !is.null(dataset.data()
                     Dataset$DO_Analysis) || Dataset$parms_loaded) {
                brushedPoints(dataset$get_parms(), USER$Selection, xvar = "Num",
                              yvar = "ETP");
            }
        }, width = 100)  ## End of output$brush_info
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
########################################
        output$parms.Download <- renderUI({
            if (Dataset$data_loaded &&  ##!is.null(dataset.data())
                Dataset$DO_Analysis) {  ##!is.null(dataset.do_analysis())
                downloadButton(outputId = "parms.Download_active",
                               "Download parameters");
            }
        })  ## End of output$parms.Download
        output$parms.Download_active <- downloadHandler(
            filename = function() paste0(Sys.Date(), "-",
                format(Sys.time(), "%H-%M-%S"), "-parameters-", dataset.fname()),
            content = function(file) {
                write.table(x = dataset$get_parms(), file = file, quote = FALSE, sep = ";");
            }
            )  ## End of output$parms.Download_active
        output$res.Download <- renderUI({
            if (Dataset$data_loaded &&  ## !is.null(dataset.data())
                Dataset$DO_Analysis) { ##!is.null(dataset.do_analysis())
                downloadButton(outputId = "res.Download_active",
                               "Download results");
                ## str(dataset$res);
            }
        })  ## End of output$res.Download
        ## Create a reactive value rf2 to store Dataset$res
        rv <- reactiveValues();
        observe({
            if (Dataset$DO_Analysis && length(Dataset$res) != 0) {
                ##!is.null(dataset.do_analysis()
                isolate(rv <<- Dataset$res)
                ## isolate(Dataset$res);
                ## print(">> from observe")
                ## print(rv)
            }
        })
        output$res.Download_active <- downloadHandler(
            filename = function() paste0(Sys.Date(), "-",
                format(Sys.time(), "%H-%M-%S"), "-results-",
                substr(dataset.fname(), 0, nchar(dataset.fname()) - 4), ".RData"),
            content = function(file) {
                save(rv, file = file);
                ## save(Dataset$res, file = file);
                ## write.table(x = dataset$parms, file = file, quote = FALSE, sep = ";")
            }
            )  ## End of output$res.Download_active
        observeEvent(  ## tracks input$dataset.overlay1
            eventExpr = input$dataset.overlay1, handlerExpr = {
                if (!is.null(input$dataset.overlay1) &&
                    input$dataset.overlay1 != "None") {
                    dataset$set_tg(input$dataset.overlay1, 1);
                    ## dataset$print_tg(which = 1);
                }
            })  ## End of observeEvent input$dataset.overlay1
        observeEvent(  ## tracks input$dataset.overlay2
            eventExpr = input$dataset.overlay2, handlerExpr = {
                if (!is.null(input$dataset.overlay2) &&
                    input$dataset.overlay2 != "None") {
                    dataset$set_tg(input$dataset.overlay2, 2);
                    ## dataset$print_tg(which = 2);
                }
            })  ## End of observeEvent input$dataset.overlay1
        observeEvent(  ## tracks Dataset$DO_Analysis to update tg1 and tg2
            eventExpr = Dataset$DO_Analysis, handlerExpr = {
                if (!is.null(Dataset$DO_Analysis) &&
                    Dataset$DO_Analysis) {
                    if (!is.null(input$dataset.overlay1) &&
                        input$dataset.overlay1 != "None") {
                        dataset$set_tg(input$dataset.overlay1, 1);
                        ## dataset$print_tg(which = 1);
                    }
                    if (!is.null(input$dataset.overlay2) &&
                        input$dataset.overlay2 != "None") {
                        dataset$set_tg(input$dataset.overlay2, 2);
                        ## dataset$print_tg(which = 2);
                    }
                }
            })  ## End of observeEvent Dataset$DO_Analysis
############################################################ Overlay tab
########################################
        output$dataset.PlotOverlay <- renderPlot({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
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
########################################
        output$dataset.PlotDrvOverlay <- renderPlot({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
                par(mfrow = c(1, 2));
                if ((Dataset$res_loaded || (Dataset$DO_Analysis)) &&
                    ## !is.null(res.data()
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
########################################
        output$dataset.ShowParmsOverlay <- renderTable({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
                if ((Dataset$res_loaded || (!is.null(Dataset$DO_Analysis) && Dataset$DO_Analysis)) &&
                    ## !is.null(res.data()
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
########################################
        output$dataset.PlotResidOverlay <- renderPlot({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
                if ((Dataset$res_loaded || (!is.null(Dataset$DO_Analysis) && Dataset$DO_Analysis)) &&
                    ## !is.null(res.data())
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None") {
                    ## par(mfrow = c(2, 1));
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
########################################
        output$dataset.ShowSmryOverlay <- renderUI({
            if (Dataset$data_loaded) {  ## !is.null(dataset.data())
                if ((Dataset$res_loaded || (!is.null(Dataset$DO_Analysis) && Dataset$DO_Analysis)) &&
                    ## !is.null(res.data())
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
######################################## Calibration signal tab
################################################################################
        cal.fname <- reactive({
            return(input$cal.fname);
        })  ## End of cal.fname
        cal.data <- reactive({
            if (!is.null(cal.fname())) {
                cal$clear();
                cal$load_signal(cal.fname());
                cal$explore_numerically();
                return(0);
            }
        })  ## End of cal.data
        cal.model <- reactive({
            cal.data();
            return(input$cal.model);
        })  ## End of cal.model
        cal.model.fit <- reactive({
            if (!is.null(cal.model()) && cal.model() != "None") {
                cal$fit_model(cal.model());
                return(0);
            } else {
                return(NULL);
            }
        })  ## End of cal.model.fit
        output$cal.Menu <- renderUI({
            if (!is.null(cal.data())) {
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
        output$cal.Plot <- renderPlot({
            if (!is.null(cal.data())) {
                cal$plot();
                if (!is.null(cal.model.fit()))
                    cal$plot_fit(cal.model());
            }
        })  ## End of output$cal.Plot
        output$cal.PlotResid <- renderPlot({
            Sys.sleep(time = 0.01);
            if (!is.null(cal.data()) && !is.null(cal.model()) &&
                cal.model() != "None" && !is.null(cal.model.fit()) &&
                cal$model_exists(cal.model())
                ## exists(cal.model(), where = cal$fit)
                ) {
                if (cal.model() != "Auto") {
                    cal$plot_residuals(cal.model());
                } else if (!cal$is_none_auto_model()) {  ## tg$fit$Auto_model != "None"
                    cal$plot_residuals("Auto");
                }
            }
        })  ## End of output$cal.PlotResid
        output$cal.model <- renderUI({
            if (!is.null(cal.model()) && cal.model() != "None" &&
                !is.null(cal.data())) {
                if (!is.null(cal.model.fit()) &&
                    cal$model_exists(cal.model())
                    ## exists(x = cal.model(), where = cal$fit)
                    ) {
                    ## if (cal.model() != "Auto") {
                    ##     x <- GetSummary(cal$fit[[cal.model()]]$smry);
                    ## } else {
                    ##     x <- GetSummary(cal$fit[[cal$fit$Auto_model]]$smry);
                    ## }
                    HTML(c("<pre>",
                           paste(cal$print_summary(cal$get_summary(cal.model())),
                                 collapse = '<br/>'), "</pre>"));
                }
            }
        })  ## End of output$cal.model
        output$cal.SynthHint <- renderTable({
            if (!is.null(cal.data()) && !is.null(cal.model()) &&
                cal.model() != "None" &&
                substr(input$cal.fname$name, 0, 9) == "Synthetic") {
                switch(cal.model(),
                       "LM" = { return(t(data.frame(
                           Parameter_Name = c("Intercept", "Slope", "Residual standard error"),
                           True_Value = c(100, 10, 2))));
                            },
                       "EarlyMM" = { return(t(data.frame(
                           Parameter_Name = c("b", "p1", "p2", "Residual standard error"),
                           True_Value = c(10, 10, 1, 2))));
                                 },
                       "LateExp" = { return(t(data.frame(
                           Parameter_Name = c("b", "p1", "p3", "Residual standard error"),
                           True_Value = c(10, 1000, 0.05, 2))));
                                 },
                       "LateMM" = { return(t(data.frame(
                           Parameter_Name = c("b", "p1", "p2", "p3", "Residual standard error"),
                           True_Value = c(10, 1500, 0.75, 0.1, 2))));
                                },
                       { ## Default
                           warning(">> Returning NULL table!");
                           return(NULL);
                       }
                       );  ## End of switch()
            }  ## End of if()
        })  ## End of output$cal.SynthHint
        cal.e0 <- reactive({
            return(input$cal.e0);
        })
        cal.s0 <- reactive({
            return(input$cal.s0);
        })
        cal.CF <- reactive({
            return(input$cal.CF);
        })
################################################################################
######################################## Thrombin generation signal tab
################################################################################
        TG <- reactiveValues(fname = NULL);
        observeEvent(  ## tracks input$tg.fname
            eventExpr = input$tg.fname, handlerExpr = {
                TG$fname <- input$tg.fname;
            })
        observeEvent(  ## tracks input$dataset.add.to.tg
            eventExpr = input$dataset.add.to.tg, handlerExpr = {
                if (input$dataset.add.to.tg != "None")
                    TG$fname <- NULL;
            })
        ## tg.clear <- observeEvent(
        ##     input$tg.clear, {
        ##         tg$clear();
        ##     })  ## End of tg.clear
        ## tg.fname <- reactive(x = {
        ##     return(input$tg.fname);
        ## })  ## End of tg.fname
        tg.data <- reactive({
            if (!is.null(TG$fname)) {  ## scenario for loading TG  !is.null(tg.fname())
                tg$clear(); tg$load_signal(TG$fname);  ## tg.fname()
                tg$explore_numerically(); tg$evaluate_numerically();
                return(0L);
            } else if (!is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                ## scenario for loading TG from dataset
                tg$clear();
                tg <<- dataset$copy_and_analyze_TG(
                    x = dataset$get_time(),
                    y = dataset$get_data_column(input$dataset.add.to.tg),
                    signal = input$dataset.add.to.tg, copy.res = FALSE);
                if (length(tg$get_num_smry()) == 0)
                    tg$explore_numerically();
                if (length(tg$get_num_eval()) == 0)
                    tg$evaluate_numerically();
                return(0L);
            } else {
                return(NULL);
            }
        })  ## End of tg.data
        tg.model <- reactive({
            if (!is.null(tg.data())) {
                ## tg.data();
                return(input$tg.model);
            } else {
                return(NULL);
            }
        })  ## End of tg.model
        tg.model.fit <- reactive({
            if (!is.null(tg.model()) && tg.model() != "None") {
                tg$fit_model(tg.model(), silent = FALSE);
                ## print(tg.model());
                ## print(tg$get_fit());
                return(0L);
            } else {
                return(NULL);
            }
        })  ## End of tg.model.fit
        output$tg.Menu <- renderUI({
            if (!is.null(tg.data())) {
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
        output$tg.Plot <- renderPlot({
            if (!is.null(tg.data())) {
                tg$plot();
                if (!is.null(tg.model.fit())) {
                    tg$plot_fit(tg.model());
                }
                ## else if (!is.null(input$dataset.add.to.tg) &&
                ##            input$dataset.add.to.tg != "None") {
                ##     ## print(tg$fit$Auto);
                ##     if (tg$fit_Auto_ok())  ## !is.null(tg$fit$Auto) && tg$fit$Auto
                ##         tg$plot_fit("Auto");
                ## }
            }
        })  ## End of output$tg.Plot
        output$tg.PlotResid <- renderPlot({
            if (!is.null(tg.model.fit()) && tg.model() != "None" &&
                tg$model_exists(tg.model())) {
                if (tg.model() != "Auto") {
                    tg$plot_residuals(tg.model());
                } else if (!tg$is_none_auto_model()) {
                    tg$plot_residuals("Auto");
                }
            }
            ## else if (!is.null(tg.data()) &&
            ##            !is.null(input$dataset.add.to.tg) &&
            ##            input$dataset.add.to.tg != "None") {
            ##     if (tg$fit_Auto_ok() && !tg$is_none_auto_model()) {
            ##         ## !is.null(tg$fit$Auto) && tg$fit$Auto && tg$fit$Auto_model != "None"
            ##         ## print(tg$fit$Auto_model); print(tg$fit$Auto);
            ##         tg$plot_residuals("Auto");
            ##     }
            ## }
        })  ## End of output$tg.PlotResid
        output$tg.model <- renderUI({
            if (!is.null(tg.model.fit()) && tg.model() != "None" &&
                tg$model_exists(tg.model())) {
                if (tg.model() != "Auto") {
                    HTML(c("<pre>",
                           paste(tg$print_summary(tg$get_summary(tg.model())),
                                 collapse = '<br/>'), "</pre>"));
                } else if (!tg$is_none_auto_model()) {
                    HTML(c("<pre>",
                           paste(tg$print_summary(tg$get_summary("Auto")),
                                 collapse = '<br/>'), "</pre>"));
                }
            }
            ## else if (!is.null(tg.data()) &&
            ##            !is.null(input$dataset.add.to.tg) &&
            ##            input$dataset.add.to.tg != "None") {
            ##     if (tg$fit_Auto_ok() & !tg$is_none_auto_model()) {
            ##         ## !is.null(tg$fit$Auto_model) && !is.null(tg$fit$Auto) &&
            ##         ## tg$fit$Auto && tg$fit$Auto_model != "None"
            ##         ## x <- GetSummary(tg$fit[[tg$fit$Auto_model]]$smry);
            ##         HTML(c("<pre>",
            ##                paste(tg$print_summary(tg$get_summary(tg$auto_model())),
            ##                      collapse = '<br/>'), "</pre>"));
            ##     }
            ## }
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
################################################################################
######################################## Thrombogram tab
################################################################################
        output$tg.PlotDrv1 <- renderPlot({
            if (!is.null(tg.data()) && tg$is_ok_num_smry()) {
                ## par(mfrow = c(2, 1));
                tg$plot_drv1();
                ## tg$plot_drv2();
                if (!is.null(tg.model.fit())) {
                    ## par(mfrow = c(1, 2));
                    tg$plot_thrombogram(tg.model());
                    ## tg$plot_velocity(tg.model());
                }
                ## else if (!is.null(tg.data()) &&
                ##            !is.null(input$dataset.add.to.tg) &&
                ##            input$dataset.add.to.tg != "None" &&
                ##            tg$is_ok_num_smry()
                ##            ## length(tg$num.smry) != 0 && length(tg$num.smry$drv1) > 1
                ##            ) {
                ##     par(mfrow = c(1, 2));
                ##     tg$plot_drv1(); tg$plot_drv2();
                ##     if (!is.null(tg$auto_model()) && !tg$is_none_auto_model()) {  ## tg$fit$Auto_model != "None"
                ##         par(mfrow = c(1, 2));
                ##         tg$plot_thrombogram("Auto"); tg$plot_velocity("Auto");
                ##     }
                ## }
            }
        })  ## End of output$PlotThromb
        output$tg.PlotDrv2 <- renderPlot({
            if (!is.null(tg.data()) && tg$is_ok_num_smry()) {
                ## par(mfrow = c(2, 1));
                ## tg$plot_drv1();
                tg$plot_drv2();
                if (!is.null(tg.model.fit())) {
                    ## par(mfrow = c(1, 2));
                    ## tg$plot_thrombogram(tg.model());
                    tg$plot_velocity(tg.model());
                }
                ## else if (!is.null(tg.data()) &&
                ##            !is.null(input$dataset.add.to.tg) &&
                ##            input$dataset.add.to.tg != "None" &&
                ##            tg$is_ok_num_smry()
                ##            ## length(tg$num.smry) != 0 && length(tg$num.smry$drv1) > 1
                ##            ) {
                ##     par(mfrow = c(1, 2));
                ##     tg$plot_drv1(); tg$plot_drv2();
                ##     if (!is.null(tg$auto_model()) && !tg$is_none_auto_model()) {  ## tg$fit$Auto_model != "None"
                ##         par(mfrow = c(1, 2));
                ##         tg$plot_thrombogram("Auto"); tg$plot_velocity("Auto");
                ##     }
                ## }
            }
        })  ## End of output$PlotThromb
################################################################################
######################################## Parameters tab
################################################################################
        output$cal.ShowParms <- renderTable({
            if (!is.null(cal.data()) && !is.null(cal.model()) &&
                cal$model_exists(cal.model())
                ## exists(x = cal.model(), where = cal$fit)
                ) {
                cal$parms_model(cal.model(), cal.e0(), cal.s0());
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$cal.ShowParms
        output$tg.ShowParms <- renderTable({
            if (!is.null(tg.data()) && !is.null(tg.model()) &&
                tg$model_exists(tg.model())
                ## exists(x = tg.model(), where = tg$fit)
                ) {
                tg$parms_model(tg.model(), cal.CF());
            } else if (!is.null(tg.data()) &&
                       !is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                tg$parms_model("Auto", cal.CF());
            } else {
                NULL;
            }
        }, digits = 3)  ## End of output$tg.ShowParms
        output$tg.ShowParmsNum <- renderTable({
            if (!is.null(tg.data())) {
                return(tg$num_parms());
            } else {
                return(NULL);
            }
        }, digits = 3)  ## End of output$tg.ShowParmsNum
################################################################################
######################################## Demo signals tab
################################################################################
        ## source("src/demo_signals_tab.R")
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
            ## } else if (substr(signal, 0, 8) == "Synthetic") {
            ##     }  ## End of if (substr())
        })  ## End of demo.signal
        ## output$demo.Show <- renderTable({
        ##     if (!is.null(demo.signal()) && demo.signal() != "None") {
        ##         demo.signal()$data;
        ##     }
        ## })  ## End of output$demo.Show
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
    }  ## End of function
)  ## End of shinyServer
################################################################################

print("######################################## >> Thrombin Analyzer reloaded! << ########################################");
source("src/libraries.R");
app.path <- getwd();  ## stores current directory
source("src/global_parameters.R"); source("src/common_functions.R");
source("src/Base_class.R"); source("src/Cal_class.R"); source("src/TG_class.R");
source("src/Dataset_class.R");

dataset <- DatasetR6$new(); cal <- Cal$new(); tg <- TG$new();
print(">> Ready to analyze!");
################################################################################
shinyServer(
    func = function(input, output) {
################################################################################
######################################## Dataset tab
################################################################################
        dataset.fname <- reactive({
            return(input$dataset.fname);
        })  ## End of dataset.fname
        dataset.data <- reactive({
            if (!is.null(dataset.fname())) {
                if (!dataset$is_empty()) {
                    dataset$clear(); r$DO <- NULL;
                }
                dataset$load(dataset.fname());
                return(0L);
            } else {
                return(NULL);
            }
        })  ## End of dataset.data
        res.fname <- reactive({
            return(input$res.fname);
        })  ## End of res.fname
        res.data <- reactive({
            if (!is.null(res.fname())) {
                dataset$load_results(res.fname());
                return(0L);
            } else {
                return(NULL);
            }
        })  ## End of res.data
        output$dataset.ShowLoadedResults <- renderUI({
            if (!is.null(res.data())) {
                ## print(length(dataset$res));
                ## print(paste0(">> Loaded ", length(dataset$res), " results"));
                HTML(c("<pre>", paste0(">> Loaded ", length(dataset$res), " results"), "</pre>"));
            }
        })  ## End of output$dataset.ShowLoadedResults
        output$dataset.Menu <- renderUI({
            if (!is.null(dataset.data())) {
                ## signals <- colnames(dataset$data)[-1];
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
            }  ## End of if ()
        })  ## End of output$dataset.Menu
        dataset.show <- reactive({
            ## print(input$dataset.show);
            if (!is.null(dataset.data())) {
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
                               dataset$plot(updateProgress, progress);
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
        r <- reactiveValues(DO = NULL, res = NULL);
        dataset.do_analysis <- observeEvent(
            eventExpr = input$dataset.analyze, handlerExpr = {
                ## builds a reactive expression that only invalidates when the
                ## value of input$goButton becomes out of date (i.e., when the
                ## button is pressed)
                if (!dataset$is_empty()) {
                    r$DO <- TRUE;
                } else {
                    r$DO <- FALSE;
                }
            })  ## End of output$dataset.do_analysis
        output$dataset.ShowParameters <- renderTable({
            if (!is.null(dataset.data()) && !is.null(r$DO) && r$DO) {  #!is.null(dataset.do_analysis())
                progress <- shiny::Progress$new();
                progress$set(message = "Analyzing dataset,", value = 0);
                on.exit(progress$close());
                dataset$do_analysis(updateProgress, progress);  ## does the auto analysis
                r$res <- dataset$get_res();
                return(summary(dataset$get_parms()));
            } else {
                ## warning(">> Data not loaded or button not pressed!");
                return(NULL);
            }
        }, digits = 6)  ## End of output$dataset.ShowParameters
########################################
        output$parms.Download <- renderUI({
            if (!is.null(dataset.data()) &&
                !is.null(r$DO) && r$DO) {  ##!is.null(dataset.do_analysis())
                downloadButton(outputId = "parms.Download_active",
                               "Download parameters now!");
            }
        })  ## End of output$parms.Download
        output$parms.Download_active <- downloadHandler(
            filename = function() paste0(Sys.Date(), "-",
                format(Sys.time(), "%H-%M-%S"), "-parameters-", dataset.fname()),
            content = function(file) {
                write.table(x = dataset$parms, file = file, quote = FALSE, sep = ";");
            }
            )  ## End of output$parms.Download_active
        output$res.Download <- renderUI({
            if (!is.null(dataset.data()) &&
                !is.null(r$DO) && r$DO) { ##!is.null(dataset.do_analysis())
                downloadButton(outputId = "res.Download_active",
                               "Download results now!");
                ## str(dataset$res);
            }
        })  ## End of output$res.Download
        ## Create a reactive value rf2 to store r$res
        rv <- reactiveValues();
        observe({
            if (!is.null(r$DO) && r$DO && length(r$res) != 0) {
                ##!is.null(dataset.do_analysis()
                isolate(rv <<- r$res)
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
                ## write.table(x = dataset$parms, file = file, quote = FALSE, sep = ";")
            }
            )  ## End of output$res.Download_active
########################################
        output$dataset.PlotOverlay <- renderPlot({
            if (!is.null(dataset.data())) {
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
            if (!is.null(dataset.data())) {
                par(mfrow = c(1, 2));
                if ((!is.null(res.data()) || (!is.null(r$DO) && r$DO)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None" &&
                    dataset$res_length_ok()
                    ## length(dataset$res) == dataset$N - 1
                    ) {
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
            if (!is.null(dataset.data())) {
                if ((!is.null(res.data()) || (!is.null(r$DO) && r$DO)) &&
                    !is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None" &&
                    ## length(dataset$res) == dataset$N - 1
                    dataset$res_length_ok()
                    ) {
                    tg <- TG$new();
                    ## tg$fit <- dataset$res[[input$dataset.overlay1]]$Auto_fit;
                    tg$set_fit(dataset$get_Auto_fit(i = input$dataset.overlay1));
                    ## tg$num.eval <- dataset$res[[input$dataset.overlay1]]$num.eval;
                    tg$set_num_eval(dataset$get_num_eval(i = input$dataset.overlay1));
                    ## table1 <- tg$parms_model(dataset$res[[input$dataset.overlay1]]$Auto_model);
                    table1 <- tg$parms_model(dataset$get_Auto_model(i = input$dataset.overlay1));
                    ## tg$fit <- dataset$res[[input$dataset.overlay2]]$Auto_fit;
                    tg$set_fit(dataset$get_Auto_fit(i = input$dataset.overlay2));
                    ## tg$num.eval <- dataset$res[[input$dataset.overlay2]]$num.eval;
                    tg$set_num_eval(dataset$get_num_eval(i = input$dataset.overlay2));
                    ## table2 <- tg$parms_model(dataset$res[[input$dataset.overlay2]]$Auto_model);
                    table2 <- tg$parms_model(dataset$get_Auto_model(i = input$dataset.overlay2));
                    ## print(table1); print(table2);
                    table12 <- data.frame(Parameter = table1$Parameter,
                                          x1 = table1$Value, x2 = table2$Value,
                                          Units = table1$Units);
                    names(table12) <- c("Parameter", input$dataset.overlay1,
                                        input$dataset.overlay2, "Units");
                    return(t(table12));
                } else {
                    return(NULL);
                }
            }
        }, digits = 6)  ## End of output$dataset.ShowParmsOverlay
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
                cal$plot_residuals(cal.model());
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
                           paste(GetSummary(cal$get_summary(cal.model())),
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
        ## tg.clear <- observeEvent(
        ##     input$tg.clear, {
        ##         tg$clear();
        ##     })  ## End of tg.clear
        tg.fname <- reactive(x = {
            return(input$tg.fname);
        })  ## End of tg.fname
        tg.data <- reactive({
            if (!is.null(tg.fname())) {
                tg$clear(); tg$load_signal(tg.fname());
                tg$explore_numerically(); tg$evaluate_numerically();
                return(0L);
            } else if (!is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                tg$clear();
                tg <<- dataset$copy_and_analyze_TG(
                    x = dataset$data[[1]],
                    y = dataset$data[[input$dataset.add.to.tg]],
                    signal = input$dataset.add.to.tg);
                if (length(tg$num.smry) == 0)
                    tg$explore_numerically();
                if (length(tg$num.eval) == 0)
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
                tg$fit_model(tg.model());
                return(0L);
            } else {
                return(NULL);
            }
        })  ## End of tg.model.fit
        output$tg.Plot <- renderPlot({
            if (!is.null(tg.data())) {
                tg$plot();
                if (!is.null(tg.model.fit())) {
                    tg$plot_fit(tg.model());
                } else if (!is.null(input$dataset.add.to.tg) &&
                           input$dataset.add.to.tg != "None") {
                    ## print(tg$fit$Auto);
                    if (!is.null(tg$fit$Auto) && tg$fit$Auto)
                        tg$plot_fit("Auto");
                }
            }
        })  ## End of output$tg.Plot
        output$tg.PlotResid <- renderPlot({
            Sys.sleep(time = 0.01);
            if (!is.null(tg.data()) && !is.null(tg.model()) &&
                tg.model() != "None" &&
                tg$model_exists(tg.model())
                ## exists(x = tg.model(), where = tg$fit)
                ) {
                if (tg.model() != "Auto") {
                    tg$plot_residuals(tg.model());
                } else if (!tg$is_none_auto_model()) {  ## tg$fit$Auto_model != "None"
                    tg$plot_residuals("Auto");
                }
            } else if (!is.null(tg.data()) &&
                       !is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                if (!is.null(tg$fit$Auto) && tg$fit$Auto && tg$fit$Auto_model != "None") {
                    print(tg$fit$Auto_model); print(tg$fit$Auto);
                    tg$plot_residuals("Auto");
                }
            }
        })  ## End of output$tg.PlotResid
        output$tg.model <- renderUI({
            if (!is.null(tg.data()) && !is.null(tg.model()) &&
                tg.model() != "None") {
                if (!is.null(tg.model.fit()) &&
                    tg$model_exists(tg.model())
                    ## exists(x = tg.model(), where = tg$fit)
                    ) {
                    HTML(c("<pre>",
                           paste(GetSummary(tg$get_summary(tg.model())),
                                 collapse = '<br/>'), "</pre>"));
                    ## if (tg.model() != "Auto") {
                    ##     x <- GetSummary(tg$fit[[tg.model()]]$smry);
                    ## } else if (tg$fit$Auto_model != "None") {
                    ##     x <- GetSummary(tg$fit[[tg$fit$Auto_model]]$smry);
                    ## } else {
                    ##     x <- NULL;
                    ## }
                    ## if (!is.null(x))
                    ##     HTML(c("<pre>", paste(x, collapse = '<br/>'), "</pre>"));
                }
            } else if (!is.null(tg.data()) &&
                       !is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                if (!is.null(tg$fit$Auto_model) && !is.null(tg$fit$Auto) &&
                    tg$fit$Auto && tg$fit$Auto_model != "None") {
                    x <- GetSummary(tg$fit[[tg$fit$Auto_model]]$smry);
                    HTML(c("<pre>", paste(x, collapse = '<br/>'), "</pre>"));
                }
            }
        })  ## End of output$tg.model
        output$tg.SynthHint <- renderTable({
            if (!is.null(input$tg.fname) && !is.null(tg.data()) && !is.null(tg.model()) &&
                tg.model() != "None" &&
                substr(input$tg.fname$name, 0, 9) == "Synthetic") {
                switch(tg.model(),
                       "Gamma" = { return(t(data.frame(
                           Parameter_Name = c("b", "A", "k", "theta", "Residual standard error"),
                           True_Value = c(50, 500, 3, 7, 2))));
                               },
                       "T0Gamma" = { return(t(data.frame(
                           Parameter_Name = c("b", "A", "k", "theta", "t0", "Residual standard error"),
                           True_Value = c(50, 500, 3, 7, 10, 2))));
                                 },
                       "GammaInt" = { return(t(data.frame(
                           Parameter_Name = c("b", "A", "k", "theta", "k.a2m", "Residual standard error"),
                           True_Value = c(10, 600, 5, 2, 0.01, 2))));
                                  },
                       "T0GammaInt" = { return(t(data.frame(
                           Parameter_Name = c("b", "A", "k", "theta", "k.a2m", "t0", "Residual standard error"),
                           True_Value = c(10, 600, 5, 2, 0.01, 10, 2))));
                                    },
                       "LateExpGammaInt" = { return(t(data.frame(
                           Parameter_Name = c("b", "p1", "A", "k", "theta", "k.a2m", "Residual standard error"),
                           True_Value = c(10, 1200, 0.5, 5, 2, 0.01, 2))));
                                         },
                       "LateExpT0GammaInt" = { return(t(data.frame(
                           Parameter_Name = c("b", "p1", "A", "k", "theta", "k.a2m", "t0", "Residual standard error"),
                           True_Value = c(10, 1200, 0.5, 5, 2, 0.01, 10, 2))));
                                           },
                       { ## Default
                           warning(">> Returning NULL table!");
                           return(NULL);
                       }
                       );  ## End of switch()
            }  ## End of if()
        })  ## End of output$tg.SynthHint
################################################################################
######################################## Thrombogram tab
################################################################################
        output$tg.PlotDrv1 <- renderPlot({
            if (!is.null(tg.data()) && tg$is_ok_num_smry()
                ## length(tg$num.smry) != 0 && length(tg$num.smry$drv1) > 1
                ) {
                par(mfrow = c(1, 2));
                tg$plot_drv1(); tg$plot_drv2();
                if (!is.null(tg.model.fit())) {
                    par(mfrow = c(1, 2));
                    tg$plot_thrombogram(tg.model()); tg$plot_velocity(tg.model());
                } else if (!is.null(tg.data()) &&
                           !is.null(input$dataset.add.to.tg) &&
                           input$dataset.add.to.tg != "None" &&
                           tg$is_ok_num_smry()
                           ## length(tg$num.smry) != 0 && length(tg$num.smry$drv1) > 1
                           ) {
                    par(mfrow = c(1, 2));
                    tg$plot_drv1(); tg$plot_drv2();
                    if (!is.null(tg$auto_model()) && !tg$is_none_auto_model()) {  ## tg$fit$Auto_model != "None"
                        par(mfrow = c(1, 2));
                        tg$plot_thrombogram("Auto"); tg$plot_velocity("Auto");
                    }
                }
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
        }, digits = 6)  ## End of output$tg.ShowParms
        output$tg.ShowParmsNum <- renderTable({
            if (!is.null(tg.data())) {
                return(tg$num_parms());
            } else {
                return(NULL);
            }
        }, digits = 6)  ## End of output$tg.ShowParmsNum
################################################################################
######################################## Demo signals tab
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
            ## } else if (substr(signal, 0, 8) == "Synthetic") {
            ##     }  ## End of if (substr())
        })  ## End of demo.signal
        output$demo.Show <- renderTable({
            if (!is.null(demo.signal()) && demo.signal() != "None") {
                demo.signal()$data;
            }
        })  ## End of output$demo.Show
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

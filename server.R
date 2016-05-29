source("src/libraries.R");
print("################################################################################");
app.path <- getwd();  ## stores current directory
source("src/global_parameters.R"); source("src/common_functions.R");
source("src/Base_class.R"); source("src/Cal_class.R"); source("src/TG_class.R");
source("src/Dataset_class.R");

dataset <- Dataset$new(); cal <- Cal$new(); tg <- TG$new();
################################################################################
shinyServer(
    func = function(input, output) {
################################################################################
######################################## TG dataset tab
################################################################################
        dataset.fname <- reactive({
            return(input$dataset.fname);
        })  ## End of dataset.fname
        dataset.data <- reactive({
            if (!is.null(dataset.fname())) {
                dataset$clear();
                dataset$load_signal(dataset.fname());
                return(0L);
            }
        })  ## End of dataset.data
        output$dataset.Menu <- renderUI({
            if (!is.null(dataset.fname()) && !is.null(dataset.data())) {
                signals <- colnames(dataset$data)[-1];
                fluidRow(
                    column(width = 6, offset = -1,
                           selectInput(inputId = "dataset.overlay1",
                                       label = h5("Overlay"),
                                       choices = as.list(c(signals, "None")),
                                       selected = "None"),
                           selectInput(inputId = "dataset.add.to.cal",
                                       label = h6("Add to calibration"),
                                       choices = as.list(c(signals, "None")),
                                       selected = "None")
                           ),
                    column(width = 6, offset = -1,
                           selectInput(inputId = "dataset.overlay2",
                                       label = h5("with"),
                                       choices = as.list(c(signals, "None")),
                                       selected = "None"),
                           selectInput(inputId = "dataset.add.to.tg",
                                       label = h6("Add to thrombin generation"),
                                       choices = as.list(c(signals, "None")),
                                       selected = "None")
                           )
                    )  ## End of fluidRow
            }  ## End of if ()
        })  ## End of output$dataset.Menu
        dataset.show <- reactive({
            ## print(input$dataset.show);
            if (!is.null(dataset.fname()) && !is.null(dataset.data())) {
                return(input$dataset.show);
            } else {
                return(NULL);
            }
        })  ## End of dataset.show
        output$dataset.ShowAs <- renderUI({
            if (!is.null(dataset.fname()) && !is.null(dataset.data()) &&
                !is.null(dataset.show())) {
                ## str(dataset$data);
                switch(dataset.show(),
                       "plot" = return(
                           tagList(renderPlot({
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
                               dataset$data;
                           })  ## End of renderTable
                                   )  ## End of tagList
                           )
                       );  ## End of switch
            } else {
                return(NULL);
            }
        })  ## End of output$dataset.ShowAs
        dataset.do_analysis <- eventReactive(
            eventExpr = input$dataset.analyze, valueExpr = {
                ## builds a reactive expression that only invalidates when the
                ## value of input$goButton becomes out of date (i.e., when the
                ## button is pressed)
                if (!is.null(dataset.fname()) && !is.null(dataset.data())) {
                    return(0L);
                } else {
                    return(NULL);
                }
            })  ## End of output$dataset.do_analysis
        ## dataset.do_analysis <- observeEvent(
        ##     eventExpr = input$dataset.analyze, handlerExpr = {
        ##         ## builds a reactive expression that only invalidates when the
        ##         ## value of input$goButton becomes out of date (i.e., when the
        ##         ## button is pressed)
        ##         if (!is.null(dataset.fname()) && !is.null(dataset.data())) {
        ##             return(0);
        ##         } else {
        ##             return(NULL);
        ##         }
        ##     })  ## End of output$dataset.do_analysis
        output$dataset.DoAnalysis <- renderTable({
            if (!is.null(dataset.fname()) && !is.null(dataset.data()) &&
                !is.null(dataset.do_analysis())) {
                progress <- shiny::Progress$new();
                progress$set(message = "Analyzing dataset,", value = 0);
                on.exit(progress$close());
                dataset$do_analysis(updateProgress, progress);  ## does the auto analysis
                ##
                x <- rep(NA, dataset$N - 1); y <- rep(NA, dataset$N - 1);
                model <- rep(NA, dataset$N - 1);
                for (i in 2:dataset$N) {
                    x[i - 1] <- dataset$res[[dataset$signals[i]]]$num.smry$rat$x;
                    y[i - 1] <- dataset$res[[dataset$signals[i]]]$num.smry$rat$y;
                    model[i - 1] <- dataset$res[[dataset$signals[i]]]$Auto_model;
                }
                return((data.frame(Signal = dataset$signals[-1], x = x, y = y,
                                  Auto_model = model)));
            } else {
                ## warning(">> Data not loaded or button not pressed!");
                return(NULL);
            }
        })  ## End of output$dataset.DoAnalysis
        output$dataset.Overlay <- renderPlot({
            if (!is.null(dataset.fname()) && !is.null(dataset.data())) {
                if (!is.null(input$dataset.overlay1) &&
                    !is.null(input$dataset.overlay2) &&
                    input$dataset.overlay1 != "None" &&
                    input$dataset.overlay2 != "None") {
                    x <- dataset$data[[1]];
                    y1 <- dataset$data[[input$dataset.overlay1]];
                    y2 <- dataset$data[[input$dataset.overlay2]];
                    graphics::plot(x, y = y1,
                                   ylim = c(
                                       min(c(min(y1, na.rm = TRUE),
                                             min(y2, na.rm = TRUE))),
                                       max(c(max(y1, na.rm = TRUE),
                                             max(y2, na.rm = TRUE)))));
                    lines(x = x, y = y2, type = "p", pch = 2);
                }
            } else {
                return(NULL);
            }
        })  ## End of output$dataset.Overlay
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
            if (!is.null(cal.data()) && !is.null(cal.model()) &&
                cal.model() != "None" && !is.null(cal.model.fit()) &&
                exists(cal.model(), where = cal$fit)) {
                cal$plot_residuals(cal.model());
            }
        })  ## End of output$cal.PlotResid
        output$cal.model <- renderUI({
            if (!is.null(cal.model()) && cal.model() != "None" &&
                !is.null(cal.data())) {
                if (!is.null(cal.model.fit()) &&
                    exists(x = cal.model(), where = cal$fit)) {
                    x <- GetSummary(cal$fit[[cal.model()]]$smry, cal.model(),
                                    full = TRUE);
                    HTML(paste(x, collapse = '<br/>'));
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
        tg.fname <- reactive(x = {
            return(input$tg.fname);
        })  ## End of tg.fname
        tg.data <- reactive({
            if (!is.null(tg.fname())) {
                tg$clear();
                tg$load_signal(tg.fname());
                tg$explore_numerically();
                tg$evaluate_numerically();
                return(0L);
            } else if (!is.null(input$dataset.add.to.tg) &&
                       input$dataset.add.to.tg != "None") {
                tg$clear();
                tg$data <- data.frame(x = dataset$data[[1]],
                                      y = dataset$data[[input$dataset.add.to.tg]]);
                str(tg$data);
                tg$explore_numerically();
                return(0L);
            }
        })  ## End of tg.data
        tg.model <- reactive({
            tg.data();
            return(input$tg.model);
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
                if (!is.null(tg.model.fit()))
                    tg$plot_fit(tg.model());
            }
        })  ## End of output$tg.Plot
        output$tg.PlotResid <- renderPlot({
            Sys.sleep(time = 0.1);
            if (!is.null(tg.data()) && !is.null(tg.model()) &&
                tg.model() != "None") {
                tg$plot_residuals(tg.model());
                ## switch(tg.model,
                ##        "Auto" = switch(tg$fit$Auto_model,
                ##            "None" = { return(NULL);},
                ##            { tg$plot_residuals(tg.model());}
                ##                        ),
                ##        { tg$plot_residuals(tg.model());}
                ##        );
                ## tg$plot_residuals(tg.model());
                ## if (tg.model() == "Auto" && tg$fit$Auto_model != "None")
                ##     tg$plot_residuals(tg.model());
            }
        })  ## End of output$tg.PlotResid
        output$tg.model <- renderUI({
            if (!is.null(tg.model()) && tg.model() != "None" &&
                !is.null(tg.data())) {
                if (!is.null(tg.model.fit()) &&
                    exists(x = tg.model(), where = tg$fit)) {
                    x <- GetSummary(tg$fit[[tg.model()]]$smry, tg.model(),
                                    full = TRUE);
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
################################################################################
######################################## Parameters tab
################################################################################
        output$cal.ShowParms <- renderTable({
            if (!is.null(cal.data()) && !is.null(cal.model()) &&  ## !is.null(cal.fname()) &&
                exists(x = cal.model(), where = cal$fit)) {
                cal$parms_model(cal.model(), cal.e0(), cal.s0());
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$cal.ShowParms
        output$tg.ShowParms <- renderTable({
            if (!is.null(tg.data()) && !is.null(tg.model()) &&  ##!is.null(tg.fname()) &&
                exists(x = tg.model(), where = tg$fit)) {
                tg$parms_model(tg.model(), cal.CF());
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$tg.ShowParms
        output$tg.ShowParmsNum <- renderTable({
            if (!is.null(tg.data())) {
                return(tg$num.eval$parms);
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

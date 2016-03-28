library(shiny); library(minpack.lm); library(LambertW);
print("################################################################################");
app.path <- getwd();  ## stores current directory
source("src/global_parameters.R");
source("src/common_functions.R");
source("src/Base_class.R");
source("src/Cal_class.R");
source("src/TG_class.R");

cal <- Cal$new(); tg <- TG$new();
################################################################################
shinyServer(
    func = function(input, output) {
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
                return(0);
            }
        })  ## End of tg.data
        tg.model <- reactive({
            tg.data();
            return(input$tg.model);
        })  ## End of tg.model
        tg.model.fit <- reactive({
            if (!is.null(tg.model()) && tg.model() != "None") {
                tg$fit_model(tg.model());
                return(0);
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
        output$tg.model <- renderUI({
            if (!is.null(tg.model()) && tg.model() != "None" &&
                !is.null(tg.data())) {
                if (!is.null(tg.model.fit()) &&
                    exists(x = tg.model(), where = tg$fit)) {
                    x <- GetSummary(tg$fit[[tg.model()]]$smry, tg.model(),
                                    full = TRUE);
                    HTML(paste(x, collapse = '<br/>'));
                }
            }
        })  ## End of output$tg.model
        output$tg.PlotResid <- renderPlot({
            if (!is.null(tg.data()) && !is.null(tg.model()) &&
                tg.model() != "None") {
                tg$plot_residuals(tg.model());
            }
        })  ## End of output$tg.PlotResid
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
            if (!is.null(cal.fname()) && !is.null(cal.data()) && !is.null(cal.model()) &&
                exists(x = cal.model(), where = cal$fit)) {
                cal$parms_model(cal.model(), cal.e0(), cal.s0());
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$cal.ShowParms
        output$tg.ShowParms <- renderTable({
            if (!is.null(tg.fname()) && !is.null(tg.data()) && !is.null(tg.model()) &&
                exists(x = tg.model(), where = tg$fit)) {
                tg$parms_model(tg.model(), cal.CF());
            } else {
                NULL;
            }
        }, digits = 6)  ## End of output$tg.ShowParms
################################################################################
######################################## Demo signals tab
################################################################################
        demo.signal <- reactive({
            signal <- input$demo.signal;
            if (substr(signal, 0, 11) == "Calibration") {
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
                       }
                       )  ## End of switch
            } else if (substr(signal, 0, 8) == "Thrombin") {
                switch(signal,
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
                       }
                       )  ## End of switch
            }
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

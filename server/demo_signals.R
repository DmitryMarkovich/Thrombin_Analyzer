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

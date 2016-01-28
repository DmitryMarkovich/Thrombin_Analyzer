require(shiny);
## code here runs once when app is started
PlotSignal <- function(var) {
    plot(var, xlab = "time, min", ylab = "Fluorescence, a.u.",
         pch = 16, type = "b", main = "Fluorescence vs time");
}  ## End of PlotSignal

################################################################################
shinyServer(
    func = function(input, output) {
        ## code here runs every time a user visits the app
        output$dataPlot <- renderPlot({
            ## code here is run every time users drags smth
            inFile <- input$data;

            if (is.null(inFile)) {
                plot(1);
            } else {
                var <- read.table(file = inFile$datapath, header = T, sep = " ");
                PlotSignal(var);
            }

        })
    }  ## End of function
)  ## End of shinyServer
################################################################################

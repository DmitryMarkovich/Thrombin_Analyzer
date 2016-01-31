################################################################################
PlotSignal <- function(env, signal.type = "tg") {
    if (any(signal.type == kPlotTypes)) {
        data.name <- kDataCodes[kPlotTypes == signal.type];
        plot(env[[paste0("data.", data.name)]], xlab = "time, min", cex.lab = 2,
             cex.axis = 2, ylab = "Fluorescence, a.u.",
             pch = 16, type = "b", main = paste0(signal.type, " signal"));
    } else {
        print(">> PlotSignal: unknown plot type supplied!");
    }
}  ## End of PlotSignal
################################################################################

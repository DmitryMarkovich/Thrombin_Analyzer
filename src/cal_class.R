Cal.plot <- function() {
    if (!is.null(data)) {
        graphics::plot(data, xlab = "time, min", cex.lab = 2, cex.axis = 2,
                       ylab = "Fluorescence, a.u.", pch = 16, type = "b",
                       main = paste0("Calibration signal"));
    } else {
        warning(">> Cal$data == NULL!");
    }
}

Cal <- setRefClass(
    Class = "Cal", contains = "Base",
    fields = list(data = "data.frame", num.smry = "list", fit = "list"),
    methods = list(plot = Cal.plot)
);

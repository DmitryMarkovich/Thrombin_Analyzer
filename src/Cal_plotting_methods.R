################################################################################
Cal.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    e0 <<- double(); s0 <<- double(); parms <<- data.frame();
}  ## End of Cal.clear
################################################################################

################################################################################
Cal.plot <- function() {
    if (length(data) != 0) {
        graphics::plot(data, xlab = "time, min", cex.lab = 2, cex.axis = 2,
                       ylab = "Fluorescence, a.u.", pch = 16, type = "b",
                       main = paste0("Calibration signal"));
        legend("bottom", leg = signif(num.smry$rat$x, 3), cex = 1, seg.len = 0,
               bty = "y", pch = NA);
        legend("top", leg = signif(num.smry$rat$y, 3), cex = 1, seg.len = 0,
               bty = "y", pch = NA);
    } else {
        warning(">> Cal$data == NULL!");
    }
}  ## End of Cal.plot
################################################################################

################################################################################
Cal.get_model <- function(calModel) {
    switch(calModel,
           "LM" = {
               ## print(">> Call to Cal.fit_LM")
               get_LM()
           },
           "EarlyMM" = {
               ## print(">> Call to Cal.get_EarlyMM")
               get_EarlyMM();
           },
           "LateExp" = {
               ## print(">> Call to Cal.get_LateExp")
               get_LateExp()
           },
           "LateMM" = {
               ## print(">> Call to Cal.get_LateMM")
               get_LateMM()
           },
           {
               print(paste0(">> Call to unknown get_model", calModel()));
           }
           );
}  ## End of Cal.get_model
################################################################################

################################################################################
Cal.plot_fit <- function(calModel) {
    plot();
    lines(data$x, get_model(calModel), col = "red", lwd = 2);
}  ## End of Cal.plot_fit
################################################################################

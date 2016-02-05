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

source("src/Cal_fitLM.R");
source("src/late_mm.R");

################################################################################
Cal.fit_model <- function(calModel) {
    switch(calModel,
           "LM" = {
               ## print(">> Call to Cal.fit_LM")
               fit_LM(calModel)
           },
           "EarlyMM" = {
               print(">> Call to Cal.fit_EarlyMM")
           },
           "LateExp" = {
               print(">> Call to Cal.fit_LateExp")
           },
           "LateMM" = {
               print(">> Call to Cal.fit_LateMM")
           },
           {
               print(paste0(">> Call to unknown model", calModel()));
           }
           );
}  ## End of Cal.fit_model
################################################################################

################################################################################
Cal.get_model <- function(calModel) {
    switch(calModel,
           "LM" = {
               ## print(">> Call to Cal.fit_LM")
               get_LM()
           },
           "EarlyMM" = {
               print(">> Call to Cal.get_EarlyMM")
           },
           "LateExp" = {
               print(">> Call to Cal.get_LateExp")
           },
           "LateMM" = {
               print(">> Call to Cal.get_LateMM")
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
    lines(data$x, get_model(calModel), col = "red");
}  ## End of Cal.plot_fit
################################################################################

################################################################################
Cal <- setRefClass(
    Class = "Cal", contains = "Base",
    fields = list(data = "data.frame", num.smry = "list", fit = "list"),
    methods = list(
        plot = Cal.plot,
        fit_LM = Cal.fitLM, get_LM = Cal.getLM,
        fit_model = Cal.fit_model, get_model = Cal.get_model,
        plot_fit = Cal.plot_fit
    )
);  ## End of Cal
################################################################################
## print(Cal);

################################################################################
Cal.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    parms <<- data.frame();
}  ## End of Cal.clear
################################################################################

################################################################################
Cal.plot <- function() {
    if (length(data) != 0) {
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
        graphics::plot(data, axes = FALSE, xlab = NA, ylab = NA, cex = 1.25, lwd = 2,
                       ylim = c(0, max(data$y))
                       );
        grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
        box();
        axis(side = 1, tck = -0.025, labels = NA);
        axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
        title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
        axis(side = 2, tck = -0.025, labels = NA, col = "black");
        axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
        title(ylab = "Fluorescence, a.u.", line = 5.5, cex.lab = 1.5);
        title(main = "Calibration signal", line = 0.5, cex.main = 1.5);
        mtext(text = as.character(signif(num.smry$rat$x, 3)), side = 1, line = -1.5, cex = 1.5);
        mtext(text = as.character(signif(num.smry$rat$y, 3)), side = 3, line = -1.5, cex = 1.5);
    } else {
        warning(">> Cal$data == NULL!");
    }
}  ## End of Cal.plot
################################################################################

################################################################################
Cal.plot_fit <- function(cal.model) {
    plot();
    lines(data$x, get_model(cal.model), col = "red", lwd = 3);
    if (any(cal.model == c("LateMM", "LateExp"))) {
        lines(data$x, get_init_rate(cal.model), col = "red", lwd = 3, lty = 2);
        legend("bottomright", legend = c("Initial rate"), pch = c(NA),
               lty = c(2), seg.len = 2.0, lwd = 4, col = c("red"), bg = "white",
               bty = "y", cex = 1.25);
    }
    if (cal.model == "Auto") {
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(cal.model, " fit");
    }
    legend("topleft",
           legend = c("Raw data", model.label),
           pch = c(1, NA, NA), lty = c(NA, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red"), bg = "white", bty = "y",
           cex = 1.25);
}  ## End of Cal.plot_fit
################################################################################

################################################################################
Cal.plot_residuals <- function(cal.model) {
    resid <- data$y - get_model(cal.model);
    graphics::plot(x = data$x, y = resid, type = "b", pch = 16, lwd = 1,
                   cex.lab = 1.5, cex.axis = 1.5, xlab = "time, min",
                   ylab = "Fluorescence, a.u.");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 0.75), mgp = c(10, 1, 0)); options(scipen = -2);
    graphics::plot(data$x, resid, axes = FALSE, xlab = NA, type = "b",
                   ylab = NA, cex = 1.25, lwd = 2);
    grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
    box();
    axis(side = 1, tck = -0.025, labels = NA);
    axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
    title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
    axis(side = 2, tck = -0.025, labels = NA, col = "black");
    axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
    title(ylab = "Fluorescence, a.u.", line = 4.5, cex.lab = 1.5);
    title(main = "Residuals of the fit", line = 0.5, cex.main = 1.5);
    abline(h = fit[[cal.model]]$smry$sigma, lwd = 3);
    abline(h = -fit[[cal.model]]$smry$sigma, lwd = 3);
    legend("top", legend = c("Residuals"), pch = c(1), lty = c(NA),
           seg.len = 0.5, lwd = 4, col = c("black"), bg = "white", bty = "y",
           cex = 1);
    legend("bottom", legend = c("Residual standard error of the fit"),
           pch = c(NA), lty = c(1), seg.len = 0.5, lwd = 4, col = c("black"),
           bg = "white", bty = "y", cex = 1);
}  ## End of Cal.plot_residuals
################################################################################

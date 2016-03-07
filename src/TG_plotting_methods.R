################################################################################
TG.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    parms <<- data.frame();
}  ## End of TG.clear
################################################################################

################################################################################
TG.plot <- function() {
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
        title(main = "Thrombin generation signal", line = 0.5, cex.main = 1.5);
        mtext(text = as.character(signif(num.smry$rat$x, 3)), side = 1, line = -1.5, cex = 1.5);
        mtext(text = as.character(signif(num.smry$rat$y, 3)), side = 3, line = -1.5, cex = 1.5);
    } else {
        warning(">> TG$data == NULL!");
    }
}  ## End of TG.plot
################################################################################

################################################################################
TG.plot_drv1 <- function() {
    ## str(data); str(num.smry);
    if (length(data) != 0 && length(num.smry) != 0) {
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        par(mar = c(4, 6, 2, 0.25), mgp = c(10, 1, 0)); options(scipen = -2);
        graphics::plot(data$x, num.smry$drv1, axes = FALSE, xlab = NA, type = "b",
                       ylab = NA, cex = 1.25, lwd = 2,
                       ylim = c(min(min(num.smry$drv1, na.rm = TRUE), 0),
                           1.1 * max(num.smry$drv1, na.rm = TRUE))
                       );
        grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
        box();
        axis(side = 1, tck = -0.025, labels = NA);
        axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
        title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
        axis(side = 2, tck = -0.025, labels = NA, col = "black");
        axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
        title(ylab = "d / dt of Fluorescence, a.u. / min",
              line = 4.5, cex.lab = 1.5);
        title(main = "Thrombogram", line = 0.5, cex.main = 1.5);
    } else {
        warning(">> data or num.smry are empty!");
    }
}  ## End of TG.plot_drv1
################################################################################

################################################################################
TG.plot_drv2 <- function() {
    ## str(data); str(num.smry);
    if (length(data) != 0 && length(num.smry) != 0) {
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        par(mar = c(4, 7, 2, 0.75), mgp = c(10, 1, 0)); options(scipen = -2);
        graphics::plot(data$x, num.smry$drv2, axes = FALSE, xlab = NA, type = "b",
                       ylab = NA, cex = 1.25, lwd = 2,
                       ylim = c(min(min(num.smry$drv2, na.rm = TRUE), 0),
                           1.1 * max(num.smry$drv2, na.rm = TRUE))
                       );
        grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
        box();
        axis(side = 1, tck = -0.025, labels = NA);
        axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
        title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
        axis(side = 2, tck = -0.025, labels = NA, col = "black");
        axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
        title(ylab = expression(paste(
            "d2 / dt"^"2", " of Fluorescence, a.u. / min"^"2")),
              line = 4.5, cex.lab = 1.5);
        title(main = "Thrombin Velocity", line = 0.5, cex.main = 1.5);
    } else {
        warning(">> data or num.smry are empty!");
    }
}  ## End of TG.plot_drv2
################################################################################

################################################################################
TG.plot_fit <- function(tg.model) {
    plot();
    lines(data$x, get_A2mT_int(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_model(tg.model), col = "red", lwd = 3);
    lines(data$x, get_thrombin_int(tg.model), col = "blue", lwd = 2);
    if (tg.model == "LateExpT0GammaInt") {
        lines(data$x, fit$LateExpT0GammaInt$cff[["b"]] +
                  get_thrombin_int(tg.model) + get_A2mT_int(tg.model), col = "orange");
    }
    if (tg.model == "Auto") {
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(tg.model, " fit");
    }
    legend("topleft",
           legend = c("Raw data", model.label, "Thrombin Integral",
               expression(paste(alpha[2], "M-T integral"))),
           pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25);
}  ## End of TG.plot_fit
################################################################################

################################################################################
TG.plot_residuals <- function(tg.model) {
    resid <- data$y - get_model(tg.model);
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
    title(main = "Residual error", line = 0.5, cex.main = 1.5);
    abline(h = fit[[tg.model]]$smry$sigma);
    abline(h = -fit[[tg.model]]$smry$sigma);
}  ## End of TG.plot_residuals
################################################################################

################################################################################
TG.plot_thrombogram <- function(tg.model) {
    plot_drv1();
    lines(data$x, get_A2mT(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_drv1(tg.model), col = "red", lwd = 3);
    lines(data$x, get_thrombin(tg.model), col = "blue", lwd = 2);
    if (tg.model == "Auto") {
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(tg.model, " fit");
    }
    legend("topright",
           legend = c("Num. Drv. 1", model.label, "Thrombin",
               expression(paste(alpha[2], "M-T"))),
           pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25);
}  ## End of TG.plot_thrombogram
################################################################################

################################################################################
TG.plot_velocity <- function(tg.model) {
    plot_drv2();
    lines(data$x, get_A2mT_vel(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_drv2(tg.model), col = "red", lwd = 3);
    lines(data$x, get_thrombin_vel(tg.model), col = "blue", lwd = 2, pch = 16);
    if (tg.model == "Auto") {
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(tg.model, " fit");
    }
    legend("topright",
           legend = c("Num. Drv. 2", model.label, "Thrombin velocity",
               expression(paste(alpha[2], "M-T velocity"))),
           pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25, ncol = 1);
}  ## End of TG.plot_thrombogram
################################################################################

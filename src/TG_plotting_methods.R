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
        abline(v = num.smry$t.lin);
        ## abline(h = num.eval$a);
        ## lines(x = data$x, y = num.eval$a + num.eval$b * (data$x - num.smry$t.lin), col = "blue");
        ## lines(x = data$x, y = num.eval$b * (data$x - num.smry$t.lin), col = "cyan");
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
        par(mar = c(4, 8, 2, 0.25), mgp = c(10, 1, 0)); options(scipen = -2);
        graphics::plot(data$x, num.smry$drv1, axes = FALSE, xlab = NA, type = "b",
                       ylab = NA, cex = 1.25, lwd = 2,
                       ylim = c(min(min(num.smry$drv1, na.rm = TRUE), 0),
                           1.25 * max(num.smry$drv1, na.rm = TRUE))
                       );
        grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
        box();
        axis(side = 1, tck = -0.025, labels = NA);
        axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
        title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
        axis(side = 2, tck = -0.025, labels = NA, col = "black");
        axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
        title(ylab = "d / dt of Fluorescence, a.u. / min",
              line = 6, cex.lab = 1.5);
        title(main = "Thrombogram", line = 0.5, cex.main = 1.5);
        abline(h = num.smry$cutoff); abline(v = num.smry$t.lin);
        ## abline(h = num.eval$b, col = "orange");
        ## lines(x = data$x, y = num.eval$A2mT, col = "cyan");
        ## lines(x = data$x, y = num.smry$drv1 - num.eval$A2mT, col = "blue");
    } else {
        warning(">> data or num.smry are empty!");
        return(NULL);
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
                           2.25 * max(num.smry$drv2, na.rm = TRUE))
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
    if (tg.model == "Auto") {
        tg.model <- fit$Auto_model;
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(tg.model, " fit");
    }
    if (any(tg.model == c("LateExpGammaInt", "LateExpT0GammaInt"))) {
        ## print(tg.model);
        lines(data$x, fit[[tg.model]]$cff[["b"]] +
                  get_thrombin_int(tg.model) + get_A2mT_int(tg.model),
              col = "orange", lwd = 2);
        legend("bottomright",
               legend = c("Compensated for \n substrate consumption"),
               pch = NA, lty = 1, seg.len = 0.5,
               lwd = 4, col = "orange", bg = "white", bty = "y", cex = 1.25);
    }
    if (tg.model == "T0GammaInt2") {
        lines(data$x, get_thrombin_int_contribution(tg.model, 1),
              col = "blue", lwd = 2, lty = 2);
        lines(data$x, get_thrombin_int_contribution(tg.model, 2),
              col = "blue", lwd = 2, lty = 3);
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
    title(main = "Residuals of the fit", line = 0.5, cex.main = 1.5);
    abline(h = fit[[tg.model]]$smry$sigma, lwd = 3);
    abline(h = -fit[[tg.model]]$smry$sigma, lwd = 3);
    legend("top", legend = c("Residuals"), pch = c(1), lty = c(NA),
           seg.len = 0.5, lwd = 4, col = c("black"), bg = "white", bty = "y",
           cex = 1);
    legend("bottom", legend = c("Residual standard error of the fit"),
           pch = c(NA), lty = c(1), seg.len = 0.5, lwd = 4, col = c("black"),
           bg = "white", bty = "y", cex = 1);
}  ## End of TG.plot_residuals
################################################################################

################################################################################
TG.plot_thrombogram <- function(tg.model) {
    plot_drv1();
    lines(data$x, get_A2mT(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_drv1(tg.model), col = "red", lwd = 3);
    lines(data$x, get_thrombin(tg.model), col = "blue", lwd = 2);
    if (tg.model == "Auto") {
        tg.model <- fit$Auto_model;
        model.label <- paste0(fit$Auto_model, " fit (A)");
    } else {
        model.label <- paste0(tg.model, " fit");
    }
    if (tg.model == "T0GammaInt2") {
        lines(data$x, get_thrombin_contribution(tg.model, 1),
              col = "blue", lwd = 2, lty = 2);
        lines(data$x, get_thrombin_contribution(tg.model, 2),
              col = "blue", lwd = 2, lty = 3);
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

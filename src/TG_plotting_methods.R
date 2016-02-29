################################################################################
TG.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    parms <<- data.frame();
}  ## End of TG.clear
################################################################################

################################################################################
TG.plot <- function() {
    if (length(data) != 0) {
        graphics::plot(data, xlab = "time, min", cex.lab = 1.5, cex.axis = 1.5,
                       ylab = "Fluorescence, a.u.", pch = 16, type = "b",
                       main = paste0("Thrombin generation signal"),
                       ylim = c(0, max(data$y)));
        mtext(text = as.character(signif(num.smry$rat$x, 3)), side = 1, line = -1);
        mtext(text = as.character(signif(num.smry$rat$y, 3)), side = 3, line = -1);
    } else {
        warning(">> TG$data == NULL!");
    }
}  ## End of TG.plot
################################################################################

################################################################################
TG.plot_drv1 <- function() {
    ## str(data); str(num.smry);
    if (length(data) != 0 && length(num.smry) != 0) {
        graphics::plot(data$x, num.smry$drv1, xlab = "time, min",
                       cex.lab = 1.05, cex.axis = 1.05,
                       ylim = c(min(num.smry$drv1, na.rm = TRUE),
                           1.1 * max(num.smry$drv1, na.rm = TRUE)),
                       ylab = "Rate of change of Fluorescence, a.u. / min", pch = 16,
                       type = "b", main = paste0("Thrombogram"));
    } else {
        warning(">> data or num.smry are empty!");
    }
}  ## End of TG.plot_drv1
################################################################################

################################################################################
TG.plot_drv2 <- function() {
    ## str(data); str(num.smry);
    if (length(data) != 0 && length(num.smry) != 0) {
        graphics::plot(data$x, num.smry$drv2, xlab = "time, min",
                       cex.lab = 1.05, cex.axis = 1.05,
                       ylim = c(min(num.smry$drv2, na.rm = TRUE),
                           2.5 * max(num.smry$drv2, na.rm = TRUE)),
                       ylab = expression(paste(
                           "Rate of change of Fluorescence, a.u. / min"^"2")),
                       pch = 16, type = "b", main = paste0("Thrombin Velocity"));
    } else {
        warning(">> data or num.smry are empty!");
    }
}  ## End of TG.plot_drv2
################################################################################

################################################################################
TG.plot_fit <- function(tg.model) {
    plot();
    lines(data$x, get_A2mT_int(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_model(tg.model), col = "red", lwd = 2);
    lines(data$x, get_thrombin_int(tg.model), col = "blue", lwd = 2);
    legend("topleft",
           legend = c("Raw data", paste0(tg.model, " fit"), "Thrombin Integral",
               expression(paste(alpha[2], "M-T integral"))),
           pch = c(16, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25);
}  ## End of TG.plot_fit
################################################################################

################################################################################
TG.plot_thrombogram <- function(tg.model) {
    plot_drv1();
    lines(data$x, get_A2mT(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_drv1(tg.model), col = "red", lwd = 2);
    lines(data$x, get_thrombin(tg.model), col = "blue", lwd = 2);
    legend("topright",
           legend = c("Raw data", paste0(tg.model, " fit"), "Thrombin",
               expression(paste(alpha[2], "M-T"))),
           pch = c(16, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25);
}  ## End of TG.plot_thrombogram
################################################################################

################################################################################
TG.plot_velocity <- function(tg.model) {
    plot_drv2();
    lines(data$x, get_A2mT_vel(tg.model), col = "cyan", lwd = 2);
    lines(data$x, get_drv2(tg.model), col = "red", lwd = 2);
    lines(data$x, get_thrombin_vel(tg.model), col = "blue", lwd = 2, pch = 16);
    legend("topright",
           legend = c("Raw data", paste0(tg.model, " fit"), "Thrombin velocity",
               expression(paste(alpha[2], "M-T velocity"))),
           pch = c(16, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 4,
           col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
           cex = 1.25, ncol = 1);
}  ## End of TG.plot_thrombogram
################################################################################

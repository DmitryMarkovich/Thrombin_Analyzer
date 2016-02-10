################################################################################
TG.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    parms <<- data.frame();
}  ## End of TG.clear
################################################################################

################################################################################
TG.plot <- function() {
    if (length(data) != 0) {
        graphics::plot(data, xlab = "time, min", cex.lab = 2, cex.axis = 2,
                       ylab = "Fluorescence, a.u.", pch = 16, type = "b",
                       main = paste0("Thrombin generation signal"));
        mtext(text = as.character(signif(num.smry$rat$x, 3)), side = 1, line = -1);
        mtext(text = as.character(signif(num.smry$rat$y, 3)), side = 3, line = -1);
    } else {
        warning(">> TG$data == NULL!");
    }
}  ## End of TG.plot
################################################################################

################################################################################
TG.plot_drv1 <- function() {
    str(data); str(num.smry);
    if (length(data) != 0 && length(num.smry) != 0) {
        graphics::plot(data$x, num.smry$drv1, xlab = "time, min",
                       cex.lab = 2, cex.axis = 2,
                       ylab = "Rate of change of Fluorescence, a.u. / min", pch = 16,
                       type = "b", main = paste0("Thrombogram"));
    } else {
        warning(">> data or num.smry are empty!");
    }
}  ## End of TG.plot_drv1
################################################################################

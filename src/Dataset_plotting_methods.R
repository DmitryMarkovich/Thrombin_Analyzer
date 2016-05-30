################################################################################
Dataset.plot <- function(updateProgress = NULL, progress) {
    if (length(data) != 0) {
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        ## par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0));
        ## N <- length(data); titles <- colnames(data);
        plot.matrix <- CalculatePlotLayout(N - 1); print(plot.matrix);
        par(mfrow = plot.matrix, mar = rep(0, 4));  ## options(scipen = -2); pin = c(0.5, 0.5)
        for (i in 2:N) {
            ## Sys.sleep(0.1);
            ## print(paste0(">> Plotting ", titles[i]));
            graphics::plot(data[[1]], data[[i]], main = NA, xlab = NA, ylab = NA,
                           cex = 0.5, cex.axis = 0.5, cex.main = 0.5, axes = FALSE,
                           ylim = c(0, max(data[[i]], na.rm = TRUE)));
            box();
            title(i - 1, line = -1.5, cex.main = 1.5);
            ## If we were passed a progress update function, call it
            if (is.function(updateProgress)) {
                text <- paste0(", compound ", i - 1, " out of ", N - 1);
                updateProgress(progress, amount = 1 / (N - 1), detail = text);
            }
        }
    } else {
        warning(">> length(data) == 0, returning NULL!");
        return(NULL);
    }
}  ## End of Dataset.plot
################################################################################

################################################################################
Dataset.plot_overlay <- function(signal1, signal2) {
    if (!is.null(signal1) && !is.null(signal2)) {
        x <- data[[1]]; y1 <- data[[signal1]]; y2 <- data[[signal2]];
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
        graphics::plot(x, y = y1,
                       ylim = c(
                           min(c(min(y1, na.rm = TRUE),
                                 min(y2, na.rm = TRUE))),
                           max(c(max(y1, na.rm = TRUE),
                                 max(y2, na.rm = TRUE)))),
                       axes = FALSE, xlab = NA, ylab = NA, cex = 1.25, lwd = 2);
        grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
        box();
        axis(side = 1, tck = -0.025, labels = NA);
        axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
        title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
        axis(side = 2, tck = -0.025, labels = NA, col = "black");
        axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
        title(ylab = "Fluorescence, a.u.", line = 5.5, cex.lab = 1.5);
        title(main = "Signals overlay plot", line = 0.5, cex.main = 1.5);
        lines(x = x, y = y2, type = "p", pch = 2, cex = 1.25, lwd = 2);
        legend("topleft", legend = c(signal1, signal2), pch = 1:2,
               cex = 1.25, seg.len = 0);
    }
}  ## End of Dataset.plot_overlay
################################################################################

################################################################################
Dataset.plot_drv1_overlay <- function(signal1, signal2) {
    if (!is.null(signal1) && !is.null(signal2)) {
        x <- data[[1]];
        if (exists(signal1, res)) {
            y1 <- res[[signal1]]$num.smry$drv1;
            if (exists(signal2, res)) {
                y2 <- res[[signal2]]$num.smry$drv1;
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(x, y = y1,
                               ylim = c(
                                   min(c(min(y1, na.rm = TRUE),
                                         min(y2, na.rm = TRUE))),
                                   max(c(max(y1, na.rm = TRUE),
                                         max(y2, na.rm = TRUE)))),
                               axes = FALSE, xlab = NA, ylab = NA, cex = 1.25, lwd = 2);
                grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                box();
                axis(side = 1, tck = -0.025, labels = NA);
                axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                axis(side = 2, tck = -0.025, labels = NA, col = "black");
                axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                title(ylab = "Rate of change of fluorescence, a.u. / min",
                      line = 5.5, cex.lab = 1.25);
                lines(x = x, y = y2, type = "p", pch = 2, cex = 1.25, lwd = 2);
                legend("topright", legend = c(signal1, signal2), pch = 1:2,
                       cex = 1.25, seg.len = 0);
            }
        }
    }
}  ## End of Dataset.plot_drv1_overlay
################################################################################

################################################################################
Dataset.plot_drv2_overlay <- function(signal1, signal2) {
    if (!is.null(signal1) && !is.null(signal2)) {
        x <- data[[1]];
        if (exists(signal1, res)) {
            y1 <- res[[signal1]]$num.smry$drv2;
            if (exists(signal2, res)) {
                y2 <- res[[signal2]]$num.smry$drv2;
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(x, y = y1,
                               ylim = c(
                                   min(c(min(y1, na.rm = TRUE),
                                         min(y2, na.rm = TRUE))),
                                   max(c(max(y1, na.rm = TRUE),
                                         max(y2, na.rm = TRUE)))),
                               axes = FALSE, xlab = NA, ylab = NA, cex = 1.25, lwd = 2);
                grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                box();
                axis(side = 1, tck = -0.025, labels = NA);
                axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                axis(side = 2, tck = -0.025, labels = NA, col = "black");
                axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                title(ylab = "Rate of change of fluorescence, a.u. / min * min",
                      line = 5.5, cex.lab = 1.25);
                lines(x = x, y = y2, type = "p", pch = 2, cex = 1.25, lwd = 2);
                legend("topright", legend = c(signal1, signal2), pch = 1:2,
                       cex = 1.25, seg.len = 0);
            }
        }
    }
}  ## End of Dataset.plot_drv2_overlay
################################################################################

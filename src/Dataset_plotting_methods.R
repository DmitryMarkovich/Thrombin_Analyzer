################################################################################
Dataset$set(
    which = "public", name = "calculate_plot_layout",
    value = compiler::cmpfun(
        f = function(N) {
            if (N != 0) {
                i1 <- round(sqrt(N)); i2 <- ceiling(sqrt(N));
                if (i1 * i2 >= N) {
                    ## if (i2 > 10) {
                    ##     return(c(ceiling(i1 * i2 / 10), 10));
                    ## } else {
                    ##     return(c(i1, i2));
                    ## }
                    return(c(i1, i2));
                } else {
                    ## if (i2 > 10) {
                    ##     return(c(ceiling((i1 + 1) * i2 / 10), 10));
                    ## } else {
                    ##     return(c(i1 + 1), i2);
                    ## }
                    return(c(i1 + 1, i2));
                }
            } else {
                warning(">> N == 0, returning NULL!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$calculate_plot_layout
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "plot",
    value = compiler::cmpfun(
        f = function(updateProgress = NULL, progress) {
            if (length(data) != 0) {
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                ## par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0));
                ## plot.matrix <- CalculatePlotLayout(N - 1);  ## print(plot.matrix);
                plot.matrix <- calculate_plot_layout(N - 1);
                par(mfrow = plot.matrix, mar = rep(0.1, 4));  ## options(scipen = -2); pin = c(0.5, 0.5)
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
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$plot
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "visualize_parameters",
    value = compiler::cmpfun(
        f = function(which = "ETP") {
            if (length(parms > 1)) {
                ## print(parms);
                ## print(parms$ETP);
                ## print(is.na(parms$ETP));
                ## par(mfrow = c(2, 3), cex.axis = 2, cex.main = 2, cex.lab = 2,
                ##     las = 1, mar = c(5, 8, 2, 0.5));
                par(cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5, las = 0,
                    mar = c(4.5, 2.5, 1.5, 0)); options(scipen = -2);
                if (!is.null(N)) {
                    x <- 1:(N - 1);
                } else {
                    x <- 1:length(parms$Signal);
                }
                if (which == "Lagtime") {
                    graphics::plot(x = x, y = parms$Lagtime,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = "Lagtime, min ",
                                   ylim = c(
                                       min(parms$Lagtime[parms$Reliable], na.rm = TRUE),
                                       max(parms$Lagtime[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
                if (which == "ETP") {
                    graphics::plot(x = x, y = parms$ETP,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = paste0("ETP, a.u. (", sum(!is.na(parms$ETP)), ")"),
                                   ylim = c(
                                       min(parms$ETP[parms$Reliable], na.rm = TRUE),
                                       max(parms$ETP[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
                if (which == "Peak") {
                    graphics::plot(x = x, y = parms$Peak,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = "Peak, a.u. / min",
                                   ylim = c(
                                       min(parms$Peak[parms$Reliable], na.rm = TRUE),
                                       max(parms$Peak[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
                if (which == "ttPeak") {
                    graphics::plot(x = x, y = parms$ttPeak,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = "ttPeak, min",
                                   ylim = c(
                                       min(parms$ttPeak[parms$Reliable], na.rm = TRUE),
                                       max(parms$ttPeak[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
                if (which == "VelIndex") {
                    graphics::plot(x = x, y = parms$VelIndex,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = "VelIndex, a.u. / min ^ 2",
                                   ylim = c(
                                       min(parms$VelIndex[parms$Reliable], na.rm = TRUE),
                                       max(parms$VelIndex[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
                if (which == "Alpha2M_Level") {
                    graphics::plot(x = x, y = parms$Alpha2M_Level,
                                   xlab = "Compound number", ylab = NA,
                                   pch = 1 + 3 * !parms$Reliable,
                                   main = "Alpha2M_Level, a.u.",
                                   ylim = c(
                                       min(parms$Alpha2M_Level[parms$Reliable], na.rm = TRUE),
                                       max(parms$Alpha2M_Level[parms$Reliable], na.rm = TRUE)
                                       )
                                   );
                }
            } else {
                print(">> length(parms) <= 1!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "plot_overlay",
    value = compiler::cmpfun(
        f =  function(signal1, signal2) {
            if (!is.null(signal1) && !is.null(signal2)) {
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(tg1$get_data()$x, y = tg1$get_data()$y,
                               ylim = c(0,
                                   ## min(c(min(y1, na.rm = TRUE),
                                   ##       min(y2, na.rm = TRUE))),
                                   max(c(max(tg1$get_data()$y, na.rm = TRUE),
                                         max(tg2$get_data()$y, na.rm = TRUE)))),
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
                lines(x = tg2$get_data()$x, y = tg2$get_data()$y, type = "p",
                      pch = 2, cex = 1.25, lwd = 2);
                legend("top", legend = c(signal1, signal2), pch = 1:2,
                       cex = 1.25, seg.len = 0.0, horiz = TRUE);

                if (length(res) > 0 && length(res) != N - 1) {
                    legend("center", legend = "You must have loaded different dataset and result files!",
                           pch = NA, text.col = 2, cex = 2, seg.len = 0);
                } else {
                    if (!is.null(res[[signal1]])) {
                        lines(tg1$get_data()$x, tg1$get_A2mT_int(res[[signal1]]$Auto_model),
                              col = "cyan", cex = 0.75, type = "b", pch = 1);
                        lines(tg1$get_data()$x, tg1$get_model(res[[signal1]]$Auto_model),
                              col = "red", lwd = 3);
                        lines(tg1$get_data()$x,
                              tg1$get_thrombin_int(res[[signal1]]$Auto_model),
                              col = "blue", cex = 0.75, type = "b", pch = 1);
                        legend("top",
                               legend = c(paste0(signal1, " (",
                                   res[[signal1]]$Auto_model, ")"), signal2),
                               pch = 1:2, cex = 1.25, seg.len = 0.0, horiz = TRUE);
                        legend("topleft",
                               legend = c("Fit", "Thrombin Integral",
                                   expression(paste(alpha[2], "M-T Integral"))),
                               fill = c("red", "blue", "cyan"),
                               bg = "white", bty = "y", cex = 1.25);
                        if (!is.null(res[[signal2]])) {
                            lines(tg2$get_data()$x, tg2$get_A2mT_int(res[[signal2]]$Auto_model),
                                  col = "cyan", cex = 0.75, type = "b", pch = 2);
                            lines(tg2$get_data()$x, tg2$get_model(res[[signal2]]$Auto_model),
                                  col = "red", lwd = 3);
                            lines(tg2$get_data()$x, tg2$get_thrombin_int(res[[signal2]]$Auto_model),
                                  col = "blue", cex = 0.75, type = "b", pch = 2);
                            legend("top", legend =
                                       c(paste0(signal1, " (", res[[signal1]]$Auto_model, ")"),
                                         paste0(signal2, " (", res[[signal2]]$Auto_model, ")")),
                                   pch = 1:2, cex = 1.25, seg.len = 0.0, horiz = TRUE);
                        }  ## End of if (exists(signal2))
                    }  ## End of if (exists(signal1))
                }  ## End of if (length(res))
            }  ## End of if (!is.null)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$plot_overlay
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "plot_drv1_overlay",
    value = compiler::cmpfun(
        f = function(signal1, signal2) {
            print(">> Dataset.plot_drv1_overlay called!");
            if (!is.null(signal1) && !is.null(signal2)) {
                ## x <- data[[1]];
                ## print(length(res));
                if (!is.null(res[[signal1]])) {  ## exists(signal1, res)
                    ## y1 <- res[[signal1]]$num.smry$drv1;
                    if (!is.null(res[[signal2]])) {  ## exists(signal2, res)
                        ## y2 <- res[[signal2]]$num.smry$drv1;
                        ## if (length(y1) > 1 && length(y2) > 1) {
                        if (tg1$is_ok_num_smry() && tg2$is_ok_num_smry()) {
                            ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                            par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                            graphics::plot(tg1$get_data()$x,
                                           y = tg1$get_num_smry()$drv1,
                                           ylim = c(
                                               min(c(min(tg1$get_num_smry()$drv1, na.rm = TRUE),
                                                     min(tg2$get_num_smry()$drv1, na.rm = TRUE))),
                                               max(c(max(tg1$get_num_smry()$drv1, na.rm = TRUE),
                                                     max(tg2$get_num_smry()$drv1, na.rm = TRUE)))),
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
                            title(main = "Thrombogram", line = 0.5, cex.main = 1.5);
                            lines(x = tg2$get_data()$x,
                                  y = tg2$get_num_smry()$drv1, type = "p",
                                  pch = 2, cex = 1.25, lwd = 2);
                            legend("topright", legend = c(signal1, signal2),
                                   pch = 1:2, cex = 1.25, seg.len = 0);

                            ## tg1 <- copy_and_analyze_TG(x = x, y = tg1$get_num_smry()$drv1, signal = signal1);
                            lines(tg1$get_data()$x, tg1$get_A2mT(res[[signal1]]$Auto_model), col = "cyan",
                                  cex = 0.5, type = "b", pch = 1);
                            lines(tg1$get_data()$x, tg1$get_drv1(res[[signal1]]$Auto_model), col = "red", lwd = 3);
                            lines(tg1$get_data()$x, tg1$get_thrombin(res[[signal1]]$Auto_model),
                                  col = "blue", cex = 0.5, type = "b", pch = 1);

                            ## tg2 <- copy_and_analyze_TG(x = x, y = tg2$get_num_smry()$drv1, signal = signal2);
                            lines(tg2$get_data()$x, tg2$get_A2mT(res[[signal2]]$Auto_model), col = "cyan",
                                  cex = 0.5, type = "b", pch = 2);
                            lines(tg2$get_data()$x, tg2$get_drv1(res[[signal2]]$Auto_model), col = "red", lwd = 3);
                            lines(tg2$get_data()$x, tg2$get_thrombin(res[[signal2]]$Auto_model),
                                  col = "blue", cex = 0.5, type = "b", pch = 2);

                            legend("right",
                                   legend = c("Fit", "Thrombin",
                                       expression(paste(alpha[2], "M-T"))),
                                   ## pch = c(NA), lty = c(1), seg.len = 0.5, lwd = 4,
                                   fill = c("red", "blue", "cyan"),
                                   bg = "white", bty = "y", cex = 1.25);
                        }  ## End of if (length)
                    }  ## End of if (exists(signal2))
                }  ## End of if (exists(signal1))
            }  ## End of if (!is.null())
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$plot_drv1_overlay
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "plot_drv2_overlay",
    value = compiler::cmpfun(
        f = function(signal1, signal2) {
            if (!is.null(signal1) && !is.null(signal2)) {
                ## x <- data[[1]];
                if (!is.null(res[[signal1]])) {  ## exists(signal1, res)
                    ## y1 <- res[[signal1]]$num.smry$drv2;
                    if (!is.null(res[[signal2]])) {  ## exists(signal2, res)
                        ## y2 <- res[[signal2]]$num.smry$drv2;
                        if (tg1$is_ok_num_smry() && tg2$is_ok_num_smry()) {
                            ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                            par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                            graphics::plot(tg1$get_data()$x, y = tg1$get_num_smry()$drv2,
                                           ylim = c(
                                               min(c(min(tg1$get_num_smry()$drv2, na.rm = TRUE),
                                                     min(tg2$get_num_smry()$drv2, na.rm = TRUE))),
                                               kYlimMultDrv2 * max(c(max(tg1$get_num_smry()$drv2, na.rm = TRUE),
                                                                     max(tg2$get_num_smry()$drv2, na.rm = TRUE)))),
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
                            title(main = "Thrombin Velocity", line = 0.5, cex.main = 1.5);
                            lines(tg2$get_data()$x, tg2$get_num_smry()$drv2,
                                  type = "p", pch = 2, cex = 1.25, lwd = 2);
                            legend("topright", legend = c(signal1, signal2), pch = 1:2,
                                   cex = 1.25, seg.len = 0);

                            ## tg1 <- copy_and_analyze_TG(x = x, y = tg1$get_num_smry()$drv2, signal = signal1);
                            lines(tg1$get_data()$x, tg1$get_A2mT_vel(res[[signal1]]$Auto_model),
                                  col = "cyan", cex = 0.5, type = "b", pch = 1);
                            lines(tg1$get_data()$x, tg1$get_drv2(res[[signal1]]$Auto_model), col = "red", lwd = 3);
                            lines(tg1$get_data()$x, tg1$get_thrombin_vel(res[[signal1]]$Auto_model),
                                  col = "blue", cex = 0.5, type = "b", pch = 1);

                            ## tg2 <- copy_and_analyze_TG(x = x, y = tg1$get_num_smry()$drv2, signal = signal2);
                            lines(tg2$get_data()$x, tg2$get_A2mT_vel(res[[signal2]]$Auto_model), col = "cyan",
                                  cex = 0.5, type = "b", pch = 2);
                            lines(tg2$get_data()$x, tg2$get_drv2(res[[signal2]]$Auto_model), col = "red", lwd = 3);
                            lines(tg2$get_data()$x, tg2$get_thrombin_vel(res[[signal2]]$Auto_model),
                                  col = "blue", cex = 0.5, type = "b", pch = 2);

                            legend("bottomright",
                                   legend = c("Fit", "Thrombin Velocity",
                                       expression(paste(alpha[2], "M-T Velocity"))),
                                   ## pch = c(NA), lty = c(1), seg.len = 0.5, lwd = 4,
                                   fill = c("red", "blue", "cyan"),
                                   bg = "white", bty = "y", cex = 1.25);
                        }  ## End of if (length)
                    }  ## End of if (exists(signal2))
                }  ## End of if (exists(signal1))
            }  ## End of if (!is.null())
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$plot_drv2_overlay
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "plot_resid_overlay",
    value = compiler::cmpfun(
        f =  function(signal1, signal2) {
            if (!is.null(signal1) && !is.null(signal2)) {
                if (length(res) > 0 && length(res) != N - 1) {
                    legend("center", legend = "You must have loaded different dataset and result files!",
                           pch = NA, text.col = 2, cex = 2, seg.len = 0);
                } else {
                    if (!is.null(res[[signal1]]) && res[[signal1]]$Auto_fit$Auto) {
                        resid1 <- tg1$get_data()$y - tg1$get_model(res[[signal1]]$Auto_model);

                        if (!is.null(res[[signal2]]) && res[[signal2]]$Auto_fit$Auto) {
                            resid2 <- tg2$get_data()$y - tg2$get_model(res[[signal2]]$Auto_model);

                            if (!(is.na(sum(resid1)) && is.na(sum(resid2)))) {
                                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                                par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0)); options(scipen = -2);
                                graphics::plot(tg1$get_data()$x, resid1, type = "b", pch = 1,
                                               ylim = get_ylim_overlay(resid1, resid2),
                                               axes = FALSE, xlab = NA, ylab = NA, cex = 1.25, lwd = 2);
                                grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                                box();
                                axis(side = 1, tck = -0.025, labels = NA);
                                axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                                title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                                axis(side = 2, tck = -0.025, labels = NA, col = "black");
                                axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                                title(ylab = "Fluorescence, a.u.", line = 5.5, cex.lab = 1.5);
                                title(main = "Residuals overlay plot", line = 0.5, cex.main = 1.5);
                                lines(tg2$get_data()$x, resid2, type = "b", pch = 2, cex = 1.25, lwd = 2);
                                abline(h = tg1$get_sigma_auto(), lwd = 3); abline(h = -tg1$get_sigma_auto(), lwd = 3);
                                points(x = rep(0, 2), y = c(tg1$get_sigma_auto(), -tg1$get_sigma_auto()), pch = 1, cex = 1.25);
                                abline(h = tg2$get_sigma_auto(), lwd = 3); abline(h = -tg2$get_sigma_auto(), lwd = 3);
                                points(x = rep(max(tg2$get_data()$x), 2),
                                       y = c(tg2$get_sigma_auto(), -tg2$get_sigma_auto()), pch = 2, cex = 1.25);                                
                            }
                        }  ## End of if (exists(signal2))
                    }  ## End of if (exists(signal1))
                }  ## End of if (length(res))
            }  ## End of if (!is.null)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$plot_resid_overlay
################################################################################

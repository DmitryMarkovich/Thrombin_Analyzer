################################################################################
TG$set(
    which = "public", name = "plot",
    value = compiler::cmpfun(
        f = function() {
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
#### ratios x, y, and t.lin visualization
                ## if (length(num.smry$rat) > 0) {
                ##     if (!is.na(num.smry$rat$x))
                ##         mtext(text = as.character(signif(num.smry$rat$x, 3)),
                ##               side = 1, line = -1.5, cex = 1.5);
                ##     if (!is.na(num.smry$rat$y))
                ##         mtext(text = as.character(signif(num.smry$rat$y, 3)),
                ##               side = 3, line = -1.5, cex = 1.5);
                ##     if (!is.na(num.smry$t.lin))
                ##         abline(v = num.smry$t.lin);
                ## }
                ## abline(h = num.eval$a);
                ## lines(x = data$x, y = num.eval$a + num.eval$b * (data$x - num.smry$t.lin), col = "blue");
                ## lines(x = data$x, y = num.eval$b * (data$x - num.smry$t.lin), col = "cyan");
            } else {
                warning(">> TG$data == NULL!");
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_drv1",
    value = compiler::cmpfun(
        f = function() {
            ## str(data); str(num.smry);
            if (length(data) != 0 && length(num.smry) != 0 && length(num.smry$drv1) > 1) {
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 8, 2, 0.25), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(data$x, num.smry$drv1, axes = FALSE, xlab = NA, type = "b",
                               ylab = NA, cex = 1.25, lwd = 2,
                               ## ylim = c(-30, 4)
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
                ## abline(h = num.smry$cutoff); abline(v = num.smry$t.lin);
                ## abline(h = num.eval$b, col = "orange");
                ## lines(x = data$x, y = num.eval$A2mT, col = "cyan");
                ## lines(x = data$x, y = num.smry$drv1 - num.eval$A2mT, col = "blue");
            } else {
                warning(">> data or num.smry are empty!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_drv1
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_drv2",
    value = compiler::cmpfun(
        f = function() {
            ## str(data); str(num.smry);
            if (length(data) != 0 && length(num.smry) != 0 && length(num.smry$drv2) > 1) {
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 7, 2, 0.75), mgp = c(10, 1, 0)); options(scipen = -2);
                graphics::plot(data$x, num.smry$drv2, axes = FALSE, xlab = NA, type = "b",
                               ylab = NA, cex = 1.25, lwd = 2,
                               ylim = c(min(min(num.smry$drv2, na.rm = TRUE), 0),
                                   kYlimMultDrv2 * max(num.smry$drv2, na.rm = TRUE))
                               );
                grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                box();
                axis(side = 1, tck = -0.025, labels = NA);
                axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                axis(side = 2, tck = -0.025, labels = NA, col = "black");
                axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                title(ylab = expression(paste(
                    "d"[2], " / dt"^"2", " of Fluorescence, a.u. / min"^"2")),
                      line = 4.5, cex.lab = 1.5);
                title(main = "Thrombin Velocity", line = 0.5, cex.main = 1.5);
            } else {
                warning(">> data or num.smry are empty!");
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_drv2
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_fit",
    value = compiler::cmpfun(
        f = function(tg.model) {
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
            if (any(tg.model == c("LateExpT0GammaInt", "LateExpT0GammaInt2"))) {
                ## print(tg.model);
                lines(data$x, fit[[tg.model]]$cff[["b"]] +
                          get_thrombin_int(tg.model) + get_A2mT_int(tg.model),
                      col = "orange", lwd = 2);
                legend("bottomright",
                       legend = c("Compensated for \n substrate consumption"),
                       pch = NA, lty = 1, seg.len = 0.5,
                       lwd = 3, col = "orange", bg = "white", bty = "y", cex = 1.25);
            }
            if (any(tg.model == c("T0GammaInt2", "T0GammaInt2_test", "LateExpT0GammaInt2"))) {
                lines(data$x, get_thrombin_int_contribution(tg.model, 1),
                      col = "blue", lwd = 2, lty = 2);
                lines(data$x, get_thrombin_int_contribution(tg.model, 2),
                      col = "blue", lwd = 2, lty = 3);
            }
            legend("topleft",
                   legend = c("Raw data", model.label, "Thrombin Integral",
                       expression(paste(alpha[2], "M-T integral"))),
                   pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 3,
                   col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
                   cex = 1.25);
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_fit
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_residuals",
    value = compiler::cmpfun(
        f = function(tg.model) {
            resid <- data$y - get_model(tg.model);
            R2 <- 1 - (sum(resid ^ 2) / sum((data$y - mean(data$y)) ^ 2));
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
            title(main =
                      paste0("Residuals of the fit, Shapiro-Wilk normality test p-value = ",
                             round(shapiro.test(resid)$p.value, digits = 3)),
                  line = 0.5, cex.main = 1.5);
            if (tg.model != "Auto") {
                abline(h = fit[[tg.model]]$smry$sigma, lwd = 3);
                abline(h = -fit[[tg.model]]$smry$sigma, lwd = 3);
            } else {
                abline(h = fit[[fit$Auto_model]]$smry$sigma, lwd = 3);
                abline(h = -fit[[fit$Auto_model]]$smry$sigma, lwd = 3);
            }
            ## legend("top", legend = c(paste0("Residuals, R2 = ", signif(R2))),
            ##        pch = c(1), lty = c(NA),
            ##        seg.len = 0.5, lwd = 3, col = c("black"), bg = "white", bty = "y",
            ##        cex = 1);
            legend("bottom", legend = c("Residual standard error of the fit"),
                   pch = c(NA), lty = c(1), seg.len = 0.5, lwd = 3, col = c("black"),
                   bg = "white", bty = "y", cex = 1);
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_residuals
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_thrombogram",
    value = compiler::cmpfun(
        f = function(tg.model) {
            if (tg.model != "None") {
                plot_drv1();
                lines(data$x, get_A2mT(tg.model), col = "cyan", lwd = 2);
                lines(data$x, get_drv1(tg.model), col = "red", lwd = 3);
                lines(data$x, get_thrombin(tg.model), col = "blue", lwd = 2);
                ## print(get_thrombin(tg.model)[length(data$x)]);
                if (tg.model == "Auto") {
                    tg.model <- fit$Auto_model;
                    model.label <- paste0(fit$Auto_model, " fit (A)");
                } else {
                    model.label <- paste0(tg.model, " fit");
                }
                ## if (any(tg.model == c("T0GammaInt2", "T0GammaInt2_test", "LateExpT0GammaInt2"))) {
                ##     lines(data$x, get_thrombin_contribution(tg.model, 1),
                ##           col = "blue", lwd = 2, lty = 2);
                ##     lines(data$x, get_thrombin_contribution(tg.model, 2),
                ##           col = "blue", lwd = 2, lty = 3);
                ## }
                legend("topright",
                       legend = c("Num. Drv. 1", model.label, "Thrombin",
                           expression(paste(alpha[2], "M-T"))),
                       pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 3,
                       col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
                       cex = 1.25);
## #### checking thrombogram vs Henrik's thrombogram
##                 ## calculate p2 from gamma fit
##                 p2 <- get_drv2(tg.model); print("p2 = ");  # print(p2);
##                 ## define function T0(k.a2m)
##                 T0 <- function(k.a2m = 0, time, p2) {
##                     return(sum(exp(k.a2m * time) * p2) -
##                            0.5 * (exp(k.a2m * time[length(time)]) * p2[length(time)] +
##                                   exp(k.a2m * time[1]) * p2[1]))
##                 }
##                 ## find the null of the T0
##                 k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; print(paste0("k.a2m from fit: ", k.a2m));
##                 res <- uniroot(f = T0, interval = c(0.5 * k.a2m, 1.5 * k.a2m),
##                                time = data$x, p2 = p2,
##                                ## f.lower = tvelpeak_est_f(t = t1, A1 = A1, A2 = A2,
##                                ##                          k1 = k1, k2 = k2,
##                                ##                          theta = theta, t0 = t0),
##                                ## f.upper = tvelpeak_est_f(t = t2, A1 = A1, A2 = A2,
##                                ##                          k1 = k1, k2 = k2,
##                                ##                          theta = theta, t0 = t0),
##                                trace = 1, extendInt = "yes");
##                 print(paste0("k.a2m from uniroot: ", res$root));
##                 ## calculate thrombogram using k.a2m from uniroot
##                 GetIntgrl <- function(t, k.a2m, time, p2, t.max) {
##                     t.prime <- time[time >= t]; p2.prime <- p2[time >= t];
##                     N.prime <- length(t.prime);
##                     if (N.prime >= 3) {
##                         return(sum(exp(k.a2m * t.prime) * p2.prime) -
##                                0.5 * (exp(k.a2m * t.prime[1]) * p2.prime[1] +
##                                       exp(k.a2m * t.prime[N.prime]) * p2.prime[N.prime])
##                                );
##                     } else if (N.prime == 2) {
##                         return(0.5 * (exp(k.a2m * t.prime[1]) * p2.prime[1] +
##                                       exp(k.a2m * t.prime[2]) * p2.prime[2]));
##                     } else {
##                         return(0.0);
##                     }
##                 }
##                 HenrThromb <- function(t, k.a2m, time, p2) {
##                     dt <- time[2] - time[1];
##                     return(-exp(-k.a2m * t) * dt *
##                            GetIntgrl(t, k.a2m, time, p2, max(time)));
##                 }
##                 henr.thromb <- rep(0.0, length = length(data$x));
##                 for (i in 1:length(data$x)) {
##                     henr.thromb[i] <- HenrThromb(data$x[i], res$root, data$x, p2);
##                 }
##                 print("henr.thromb = "); # print(henr.thromb);
##                 lines(data$x, log(henr.thromb), col = "green", lwd = 4, lty = 3);
##                 print("henr.thromb / thromb = "); #print(henr.thromb / get_thrombin(tg.model));
## ######### Timing of thrombogram calculation
##                 ## define function to calculate thrombogram as Henrik
##                 get_thrombin_Henrik <- function() {
##                     p2 <- get_drv2(tg.model);
##                     res <- uniroot(f = T0, interval = c(0.5 * k.a2m, 1.5 * k.a2m),
##                                    time = data$x, p2 = p2,
##                                    ## f.lower = tvelpeak_est_f(t = t1, A1 = A1, A2 = A2,
##                                    ##                          k1 = k1, k2 = k2,
##                                    ##                          theta = theta, t0 = t0),
##                                    ## f.upper = tvelpeak_est_f(t = t2, A1 = A1, A2 = A2,
##                                    ##                          k1 = k1, k2 = k2,
##                                    ##                          theta = theta, t0 = t0),
##                                    trace = 1, extendInt = "yes");
##                     ## henr.thromb <- rep(0.0, length = length(data$x));
##                     ## for (i in 1:length(data$x)) {
##                     ##     henr.thromb[i] <- HenrThromb(data$x[i], res$root,
##                     ##                                  data$x, p2);
##                     ## }
##                 }
##                 print(microbenchmark::microbenchmark(
##                     get_thrombin(tg.model),
##                     get_thrombin_Henrik(),
##                     times = 100
##                 ));
## ######### Trying to fit data with pade
##                 fit_Pade();
            }  ## End of if (tg.model)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_thrombogram
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_velocity",
    value = compiler::cmpfun(
        f = function(tg.model) {
            if (tg.model != "None") {
                plot_drv2();
                lines(data$x, get_A2mT_vel(tg.model), col = "cyan", lwd = 2);
                lines(data$x, get_drv2(tg.model), col = "red", lwd = 3);
                lines(data$x, get_thrombin_vel(tg.model), col = "blue", lwd = 2, pch = 16);
                if (any(tg.model == c("T0GammaInt2", "T0GammaInt2_test", "LateExpT0GammaInt2"))) {
                    lines(data$x, get_thrombin_vel_contribution(tg.model, 1),
                          col = "blue", lwd = 2, lty = 2);
                    lines(data$x, get_thrombin_vel_contribution(tg.model, 2),
                          col = "blue", lwd = 2, lty = 3);
                }
                if (tg.model == "Auto") {
                    model.label <- paste0(fit$Auto_model, " fit (A)");
                } else {
                    model.label <- paste0(tg.model, " fit");
                }
                legend("topright",
                       legend = c("Num. Drv. 2", model.label, "Thrombin Velocity",
                           expression(paste(alpha[2], "M-T Velocity"))),
                       pch = c(1, NA, NA, NA), lty = c(NA, 1, 1, 1), seg.len = 0.5, lwd = 3,
                       col = c("black", "red", "blue", "cyan"), bg = "white", bty = "y",
                       cex = 1.25, ncol = 1);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_thrombogram
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_periodogram",
    value = compiler::cmpfun(
        f = function(signal = NULL, reflect = FALSE) {
            if (length(data) != 0 && length(num.smry) != 0 && length(num.smry$drv1) > 1) {
                if (is.null(signal)) {
                    fl <- data$y;
                } else {
                    fl <- signal;
                }
                N <- length(fl);  ## str(fl); print(N);
                if (reflect) {
                    fl <- c(fl, fl[(N - 1):2]);
                    N <- 2 * N - 2;
                }
                ## str(fl); print(N);
                dt <- 60 * (data$x[2] - data$x[1]);
                fl.fft <- waved::fftshift(fft(fl)) / (N * dt);
                if (N %% 2 == 0) {
                    ## print(">> using even (N)");
                    f <- (1 / dt) * ((-N / 2):(-1 + N / 2)) / N; ## print(f);
                } else {
                    ## print(">> using odd (N - 1)");
                    f <- (1 / dt) * ((-1 - (N - 1) / 2):(-1 + (N - 1) / 2)) / N; ## print(f);
                }
                f[f <= 0] <- NA_real_; pos <- complete.cases(f);
                return(data.frame(f = f[pos], prd = abs(fl.fft[pos]) ^ 2,
                                  fl.fft = fl.fft[pos]));
            } else {
                warning(">> data or num.smry are empty!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_periodogram
################################################################################

################################################################################
TG$set(
    which = "public", name = "plot_periodogram",
    value = compiler::cmpfun(
        f = function() {
            ## str(data); str(num.smry);
            if (length(data) != 0 && length(num.smry) != 0 && length(num.smry$drv1) > 1) {
                ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
                par(mar = c(4, 6, 2, 0.75), mgp = c(10, 1, 0), mfrow = c(1, 3)); options(scipen = -2);
                if (TRUE) {
                    ## plot raw data
                    graphics::plot(data$x, data$y, axes = FALSE, xlab = NA, type = "b",
                                   ylab = NA, cex = 1, lwd = 1, pch = 16);
                    grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                    box();
                    axis(side = 1, tck = -0.025, labels = NA);
                    axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                    title(xlab = "time, min", line = 2.25, cex.lab = 1.5);
                    axis(side = 2, tck = -0.025, labels = NA, col = "black");
                    axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                    title(ylab = "Fluorescence, a.u.", line = 4.5, cex.lab = 1.5);
                    ## fit <- fit_lowess(); ## fit <- fit_Pade43(); ##print(fit);
                    fit_T0GammaInt(); ft <- get_T0GammaInt();
                    thr <- get_thrombin_int("T0GammaInt");
                    a2m <- get_A2mT_int("T0GammaInt");
                    lines(data$x, ft, col = 2, lwd = 2);
                    lines(data$x, thr, col = "blue", lwd = 2);
                    lines(data$x, a2m, col = "cyan", lwd = 2);
                    legend("topleft", leg = c("Raw data", "Fit"),
                           pch = c(16, NA), lty = 1, col = c(1, 2), bg = "white",
                           lwd = 2, cex = 2);
                    ## graphics::plot(data$x, data$y - fit$y);
                }

                reflect <- TRUE;
                prd <- get_periodogram(reflect = reflect);
                prd.base <- get_periodogram(ft, reflect);
                prd.thr <- get_periodogram(thr, reflect);
                prd.a2m <- get_periodogram(a2m, reflect);
                AddFreqLine <- function(f = 1/60, log10 = FALSE) {
                    if (log10) {
                        tmp <- log(f, 10);
                    } else {
                        tmp <- f;
                    }
                    abline(v = tmp, lty = 2, lwd = 2, col = 8);
                    text(x = tmp, y = 3.5e-5,
                         label = as.character(MASS::fractions(f)), srt = 90, cex = 2.5);
                }
                if (TRUE) {
                    ## plot periodogram
                    graphics::plot(prd$f, prd$prd, axes = FALSE, xlab = NA,
                                   type = "b",
                                   ylab = NA, cex = 1, lwd = 1, pch = 16,
                                   ylim = c(0, 4e-5)
                                   ## ylim = c(1e-3, 2e-2)
                                   );
                    grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                    box();
                    axis(side = 1, tck = -0.025, labels = NA);
                    axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                    title(xlab = "Frequency, Hz", line = 2.25, cex.lab = 1.5);
                    axis(side = 2, tck = -0.025, labels = NA, col = "black");
                    axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                    title(ylab = "Periodogram, a.u.", line = 4.5, cex.lab = 1.5);
                    lines(prd.base$f, prd.base$prd, col = 2, type = "p",
                          pch = 16, lty = 2);
                    AddFreqLine(1/60); AddFreqLine(1/90);
                    lines(prd$f, prd$prd, type = "p", pch = 16, cex = 1.25);
                }
                ## plot periodogram on log10
                if (TRUE) {
                    pos <- prd$f > 0;
                    graphics::plot(prd$f[pos], log10(prd$prd[pos]), axes = FALSE,
                                   xlab = NA, type = "b",
                                   ylab = NA, cex = 1, lwd = 1,
                                   ylim = c(-20, 2), ##-20
                                   ## ylim = c(-3, 2),
                                   pch = 16); ##, log = "x"
                    grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                    box();
                    axis(side = 1, tck = -0.025, labels = NA);
                    axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                    title(xlab = "Frequency, Hz", line = 2.25, cex.lab = 1.5);
                    axis(side = 2, tck = -0.025, labels = NA, col = "black");
                    axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                    title(ylab = "log10(Periodogram), a.u.", line = 3, cex.lab = 1.5);
                    lines(prd.base$f[pos], log(prd.base$prd[pos], 10), col = 2,
                          type = "p", pch = 16, lty = 2, cex = 1.25); ##, log = "x"
                    AddFreqLine(1/60); AddFreqLine(1/90);
                    lines(prd$f[pos], log(prd$prd[pos], 10), type = "p",
                          pch = 16, cex = 1.25); ##, log = "x"
                    lines(prd.thr$f[pos], log(prd.thr$prd[pos], 10), col = "blue",
                          type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.0);
                    lines(prd.a2m$f[pos], log(prd.a2m$prd[pos], 10), col = "cyan",
                          type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.0);
                    legend("bottomleft", leg = c("Raw data", "Fit"),
                           pch = c(16, 16), lty = c(1, NA), col = c(1, 2),
                           bg = "white", cex = 2, lwd = 2);
                }
                ## print(">> Raw data");
                ## print(Im(prd$fl.fft));
                ## print(">> Fit");
                ## print(summary(Im(prd.base$fl.fft)));
                ## if (FALSE) {
                ##     graphics::plot(prd.base$f, log(abs(Re(prd.base$fl.fft)), 10),
                ##                    axes = FALSE, xlab = NA,
                ##                    ylab = NA, pch = 16, las = 1, ylim = c(-8, 1),
                ##                    cex = 1.5, col = 2);
                ##     grid(nx = NULL, ny = NULL, lty = 2, col = "black", lwd = 1);
                ##     box();
                ##     axis(side = 1, tck = -0.025, labels = NA);
                ##     axis(side = 1, lwd = 0, line = 0.15, cex.axis = 1.5);
                ##     title(xlab = "Frequency, Hz", line = 2.25, cex.lab = 1.5);
                ##     axis(side = 2, tck = -0.025, labels = NA, col = "black");
                ##     axis(side = 2, lwd = 0, line = -0.4, las = 1, cex.axis = 1.5);
                ##     title(ylab = "log10(FFT), a.u.", line = 3, cex.lab = 1.5);
                ##     ## lines(prd.base$f, log(abs(Im(prd.base$fl.fft)), 10), col = 2, type = "p", pch = 17, cex = 1.5);
                ##     ## lines(prd$f, log(abs(Re(prd$fl.fft)), 10), col = 1, type = "p", pch = 16, cex = 1.25);
                ##     ## lines(prd$f, log(abs(Im(prd$fl.fft)), 10), type = "p", pch = 17, cex = 1.25, col = 1);                    
                ##     legend("topright", leg = c("Raw data", "Fit"),
                ##            fill = c(1, 2), bg = "white", cex = 2);
                ##     ## legend("bottomleft", leg = c("abs(Re)", "abs(Im)"),
                ##     ##        pch = c(16, 17), bg = "white", cex = 2);
                ## }
            } else {
                warning(">> data or num.smry are empty!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$plot_periodogram
################################################################################

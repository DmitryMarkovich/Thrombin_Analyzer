################################################################################
TG.fit_T0GammaInt2 <- function(silent = TRUE) {
################################################################################
    if (!silent)
        print(">> fit_T0GammaInt2 called!");
    if (exists(x = "T0GammaInt2", where = fit)) {
        warning(">> No fitting: T0GammaInt2 fit already exists!");
        return(fit$T0GammaInt2);
    } else {
        if (!exists(x = "T0GammaInt", where = fit)) {
            fit_T0GammaInt(silent = TRUE);
        }
        ## print(fit$T0GammaInt$smry);
        start.list <- list(
            b = fit$T0GammaInt$cff[["b"]],
            A1 = 0.5 * fit$T0GammaInt$cff[["A"]],
            A2 = 0.5 * fit$T0GammaInt$cff[["A"]],
            k1 = fit$T0GammaInt$cff[["k"]], k2 = fit$T0GammaInt$cff[["k"]],
            theta = fit$T0GammaInt$cff[["theta"]],
            k.a2m = fit$T0GammaInt$cff[["k.a2m"]],
            t0 = fit$T0GammaInt$cff[["t0"]]
            );
        if (start.list$k1 < 2) {
            start.list$k1 <- 2.5;
        }
        if (start.list$k2 < 2) {
            start.list$k2 <- 2.5;
        }
        ## print(start.list);

        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= kNumTries) {
            ## print(paste0(">> Fit try ", n.try, " of ", kNumTries, " with start.list = "));
            ## print(start.list);
            try(expr = {
                ft <- suppressWarnings(nlsLM(
                    y ~ b + A1 * pgamma(q = x - t0, shape = k1, scale = theta) +
                        A2 * pgamma(q = x - t0, shape = k2, scale = theta) +
                        (A1 * k.a2m / gamma(k1)) * (
                            gamma(k1) * pgamma(q = x - t0, shape = k1, scale = theta) * (x - t0) -
                                gamma(k1 + 1) * theta *
                                    pgamma(q = x - t0, shape = k1 + 1, scale = theta)) +
                            (A2 * k.a2m / gamma(k2)) * (
                                gamma(k2) * pgamma(q = x - t0, shape = k2, scale = theta) * (x - t0) -
                                    gamma(k2 + 1) * theta *
                                        pgamma(q = x - t0, shape = k2 + 1, scale = theta)),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min),
                    ## upper = c(b.max, A.max, k.max, theta.max),
                    lower = c(  0,   0,   0, 2.1, 2.1,   0, 1e-5,   0),
                    upper = c(Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf),
                    algorithm = "LM",
                    control = nls.lm.control(
                        ftol = sqrt(.Machine$double.eps),
                        ptol = sqrt(.Machine$double.eps),
                        gtol = 0, factor = 50,  ## between [0.1, 100]
                        maxiter = 200, nprint = -1
                    )
                ))
            }, silent = silent);
            if (!is.null(ft)) {
                if (!silent)
                    print(">> Fit not NULL, checking dgn = ");
                dgn <- conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4]);  ## print(dgn);
                if (dgn[1] <= "5") {
                    if (!silent)
                        print(">> dgn[1] <= 5, setting ft back to NULL");
                    ft <- NULL;
                }
            }
            n.try <- n.try + 1;
            start.list <- list(
                b = runif(1, 0.9, 1.1) * start.list$b,
                A1 = runif(1, 0.5, 1.5) * start.list$A1,
                A2 = runif(1, 0.5, 1.5) * start.list$A2,
                k1 = runif(1, 3, 10), k2 = runif(1, 3, 10),
                theta = runif(1, 3, 10),
                k.a2m = runif(1, 0.5, 1.5) * start.list$k.a2m,
                t0 = runif(1, 0.9, 1.1) * start.list$t0
                );
        }  ## End of while()
        if (is.null(ft)) {
            warning(">> fit_T0GammaInt2 resulted in NULL!");
            return(NULL);
        } else {
            print(paste0(">> T0GammaInt2 started at n.try = ", n.try,
                         " with start.list = "));
            print(unlist(start.list));
            fit$T0GammaInt2 <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$T0GammaInt2);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitT0GammaInt2
################################################################################

################################################################################
TG.get_T0GammaInt2 <- function() {
    if (exists(x = "T0GammaInt2", where = fit)) {
        b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
        A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
        k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
        k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
        return(b + A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
                   A2 * pgamma(q = data$x - t0, shape = k2, scale = theta) +
                   (A1 * k.a2m / gamma(k1)) * (
                       gamma(k1) *
                           pgamma(q = data$x - t0, shape = k1, scale = theta) * (data$x - t0) -
                               gamma(k1 + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k1 + 1, scale = theta)) +
                       (A2 * k.a2m / gamma(k2)) * (
                           gamma(k2) *
                               pgamma(q = data$x - t0, shape = k2, scale = theta) * (data$x - t0) -
                                   gamma(k2 + 1) * theta *
                                       pgamma(q = data$x - t0, shape = k2 + 1, scale = theta))
               );
    } else {
        warning(">> fit$T0GammaInt2 does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of TG_get_T0GammaInt2
################################################################################

################################################################################
TG.parms_T0GammaInt2 <- function(cal.CF = 1) {
    ## print(">> Call to TG.parms_T0GammaInt2");
    if (exists(x = "T0GammaInt2", where = fit)) {
        b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
        A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
        k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
        k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
        ## numerical estimation
        ## time <- seq(from = 0.75 * num.smry$t.peak, to = 1.25 * num.smry$t.peak,
        ##             by = 0.01);
        ## fl <- get_thrombin("T0GammaInt2", time);
        ## peak <- max(fl, na.rm = TRUE); t.peak <- time[fl == peak];
        ## vel.index <- max(get_thrombin_vel("T0GammaInt2", seq(t0, t.peak, by = 0.01)),
        ##                  na.rm = TRUE);
        ## print(">> Numerical estimation");
        ## print(peak); print(t.peak); print(vel.index);
        ## analytical result
        ## print(">> Analytical result");
        peak1 <- GetPeak(A1, k1, theta); peak2 <- GetPeak(A2, k2, theta);
        if (peak1 > peak2) {
            t.peak <- t0 + theta * (k1 - 1);
            peak <- peak1 + get_thrombin_contribution("T0GammaInt2", number = 2,
                                                      time = t.peak);
            t.vel.peak <- t0 + theta * (k1 - 1 - sqrt(k1 - 1));
            vel.index <- GetVelPeak(A1, k1, theta) +
                get_thrombin_vel_contribution("T0GammaInt2", number = 2,
                                              time = t.vel.peak);
        } else {
            t.peak <- t0 + theta * (k2 - 1);
            peak <- peak2 + get_thrombin_contribution("T0GammaInt2", number = 1,
                                                      time = t.peak);
            t.vel.peak <- t0 + theta * (k2 - 1 - sqrt(k2 - 1));
            vel.index <- GetVelPeak(A2, k2, theta) +
                get_thrombin_vel_contribution("T0GammaInt2", number = 1,
                                              time = t.vel.peak);
        }
        ## print(peak); print(t.peak); print(vel.index);
        ## peak <-

        ## if (k > 2) {
        ##     v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
        ##         exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
        ## } else {
        ##     v <- max(num.smry$drv2, na.rm = TRUE);
        ## }
        if (cal.CF != 1) {
            CF <- cal.CF;
            return(parms <<- data.frame(
                Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                    "Alpha2M_Level"),
                Value = c(
                    t0,
                    CF * (A1 + A2),
                    CF * peak,
                    t.peak,
                    CF * vel.index,
                    CF * k.a2m * (A1 + A2)
                    ),
                ## StdErr = rep(NA, 5),
                Units = c("min", "nM * min", "nM", "min", "nM / min", "nM"))
                   );
        } else {
            ## print(paste0("vel.index = ", vel.index));
            return(parms <<- data.frame(
                Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                    "Alpha2M_Level"),
                Value = c(
                    t0,
                    A1 + A2,
                    peak,
                    t.peak,
                    vel.index,
                    k.a2m * (A1 + A2)
                    ),
                Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                    "a.u. / min"))
                   );
        }
    } else {
        warning(">> fit$T0GammaInt2 does not exist!");
        return(NULL);
    }
}  ## End of TG.parms_T0GammaInt2
################################################################################

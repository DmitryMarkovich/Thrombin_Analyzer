################################################################################
TG.fit_T0GammaInt <- function(silent = TRUE) {
################################################################################
    if (exists(x = "T0GammaInt", where = fit)) {
        warning(">> No fitting: T0GammaInt fit already exists!");
    } else {
        if (exists(x = "GammaInt", where = fit)) {
            start.list <- list(
                b = fit$GammaInt$cff[["b"]], A = fit$GammaInt$cff[["A"]],
                k = fit$GammaInt$cff[["k"]], theta = fit$GammaInt$cff[["theta"]],
                k.a2m = fit$GammaInt$cff[["k.a2m"]], t0 = 0);
        } else {
            fit_GammaInt(silent = TRUE);
            start.list <- list(
                b = fit$GammaInt$cff[["b"]], A = fit$GammaInt$cff[["A"]],
                k = fit$GammaInt$cff[["k"]], theta = fit$GammaInt$cff[["theta"]],
                k.a2m = fit$GammaInt$cff[["k.a2m"]], t0 = 0);
        }
        ## ft <- lm(y ~ x, data = data, subset = x >= num.smry$t.lin);
        ## A <- coef(ft)[[1]] + coef(ft)[[2]] * num.smry$t.lin;
        ## k.a2m <- coef(ft)[[2]] / A; k <- 3; theta <- num.smry$t.peak / (k - 1);
        ## start.list <- list(b = data$y[1], A = A, k = k,
        ##                                theta = theta, k.a2m = k.a2m);
        ## print(start.list);

        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= N.tries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta) +
                        (A * k.a2m / gamma(k)) * (
                            gamma(k) * pgamma(q = x - t0, shape = k, scale = theta) * (x - t0) -
                                gamma(k + 1) * theta *
                                    pgamma(q = x - t0, shape = k + 1, scale = theta)),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min),
                    ## upper = c(b.max, A.max, k.max, theta.max),
                    lower = c(  0,   0, 1.5,   0,   0,   0),
                    upper = c(Inf, Inf, Inf, Inf, Inf, Inf),
                    algorithm = "LM",
                    control = nls.lm.control(
                        ftol = sqrt(.Machine$double.eps),
                        ptol = sqrt(.Machine$double.eps),
                        gtol = 0, factor = 100,  ## between [0.1, 100]
                        maxiter = 200, nprint = -1
                    )
                )
            }, silent = FALSE);
            n.try <- n.try + 1;
            start.list <- list(b = data[[2]][1], A = runif(1) * num.smry$ampl,
                               k = runif(1, 1, 10), theta = runif(1, 1, 30),
                               k.a2m = runif(1) * 1e-3, t0 = 0);
        }
        if (is.null(ft)) {
            warning(">> fit_T0GammaInt resulted in NULL!");
        } else {
            fit$T0GammaInt <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitT0GammaInt
################################################################################

################################################################################
TG.get_T0GammaInt <- function() {
    if (exists(x = "T0GammaInt", where = fit)) {
        b <- fit$T0GammaInt$cff[["b"]]; A <- fit$T0GammaInt$cff[["A"]];
        k <- fit$T0GammaInt$cff[["k"]]; theta <- fit$T0GammaInt$cff[["theta"]];
        k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
        return(b + A * pgamma(q = data$x - t0, shape = k, scale = theta) +
                   (A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k + 1, scale = theta))
               );
    } else {
        warning(">> fit$T0GammaInt does not exist!");
    }
}  ## End of TG_get_T0GammaInt
################################################################################

################################################################################
TG.get_T0GammaInt_thrombin_int <- function() {
    if (exists(x = "T0GammaInt", where = fit)) {
        A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
        theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
        return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
    } else {
        warning(">> fit$T0GammaInt does not exist!");
    }
}  ## End of TG_get_T0GammaInt_thrombin_int
################################################################################

################################################################################
TG.get_T0GammaInt_A2mT_int <- function() {
    if (exists(x = "T0GammaInt", where = fit)) {
        A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
        theta <- fit$T0GammaInt$cff[["theta"]];
        k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
        return((A * k.a2m / gamma(k)) * (
            gamma(k) *
                pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                    gamma(k + 1) * theta *
                        pgamma(q = data$x - t0, shape = k + 1, scale = theta)));
    } else {
        warning(">> fit$T0GammaInt does not exist!");
    }
}  ## End of TG_get_T0GammaInt_A2mT_int
################################################################################

################################################################################
TG.parms_T0GammaInt <- function(cal.CF) {
    print(">> Call to TG.parms_T0GammaInt");
    if (exists(x = "T0GammaInt", where = fit)) {
        A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
        theta <- fit$T0GammaInt$cff[["theta"]];
        if (k > 2) {
            v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
        } else {
            v <- max(num.smry$drv2, na.rm = TRUE);
        }
        if (cal.CF != 1) {
            CF <- cal.CF;
            return(parms <<- data.frame(
                Parameter = c("ETP", "Peak", "ttPeak", "Vel Index", "Lagtime"),
                Value = c(
                    CF * A,
                    CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    CF * v,
                    fit$T0GammaInt$cff[["t0"]]),
                StdErr = rep(NA, 5),
                Units = c("nM * min", "nM", "min", "nM / min", "min"))
                   );
        } else {
            return(parms <<- data.frame(
                Parameter = c("ETP", "Peak", "ttPeak", "Vel Index", "Lagtime"),
                Value = c(
                    A,
                    A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    v,
                    fit$T0GammaInt$cff[["t0"]]),
                StdErr = rep(NA, 5),
                Units = c("a.u.", "a.u. / min", "min", "a.u. / min * min", "min"))
                   );
        }
    } else {
        warning(">> fit$T0GammaInt does not exist!");
    }
}  ## End of TG.parms_T0GammaInt
################################################################################
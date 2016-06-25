################################################################################
TG.fit_GammaInt <- function(silent = TRUE) {
################################################################################
    if (exists(x = "GammaInt", where = fit)) {
        warning(">> No fitting: GammaInt fit already exists!");
        return(fit$GammaInt);
    } else {
        ft <- lm(y ~ x, data = data, subset = x >= num.smry$t.lin);
        A <- coef(ft)[[1]] + coef(ft)[[2]] * num.smry$t.lin;
        k.a2m <- coef(ft)[[2]] / A; k <- 3; theta <- num.smry$t.peak / (k - 1);

        ft <- NULL; start.list <- list(b = data$y[1], A = A, k = k,
                                       theta = theta, k.a2m = k.a2m);
        ## print(start.list);
        n.try <- 1;
        while (is.null(ft) && n.try <= kNumTries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + A * pgamma(q = x, shape = k, scale = theta) +
                        (A * k.a2m / gamma(k)) * (
                            gamma(k) * pgamma(q = x, shape = k, scale = theta) * x -
                                gamma(k + 1) * theta *
                                    pgamma(q = x, shape = k + 1, scale = theta)),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min),
                    ## upper = c(b.max, A.max, k.max, theta.max),
                    lower = c(  0,   0, 1.5,   0,   0),
                    upper = c(Inf, Inf, Inf, Inf, Inf),
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
                               k.a2m = runif(1) * 1e-3);
        }
        if (is.null(ft)) {
            warning(">> fit_GammaInt resulted in NULL!");
            return(NULL);
        } else {
            fit$GammaInt <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$GammaInt);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitGammaInt
################################################################################

################################################################################
TG.get_GammaInt <- function() {
    if (exists(x = "GammaInt", where = fit)) {
        b <- fit$GammaInt$cff[["b"]]; A <- fit$GammaInt$cff[["A"]];
        k <- fit$GammaInt$cff[["k"]]; theta <- fit$GammaInt$cff[["theta"]];
        k.a2m <- fit$GammaInt$cff[["k.a2m"]];
        return(b + A * pgamma(q = data$x, shape = k, scale = theta) +
                   (A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x, shape = k, scale = theta) * data$x -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x, shape = k + 1, scale = theta))
               );
    } else {
        warning(">> fit$GammaInt does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of TG_get_GammaInt
################################################################################

################################################################################
TG.parms_GammaInt <- function(cal.CF) {
    print(">> Call to TG.parms_GammaInt");
    if (exists(x = "GammaInt", where = fit)) {
        A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
        theta <- fit$GammaInt$cff[["theta"]];
        if (k > 2) {
            v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
        } else {
            v <- max(num.smry$drv2, na.rm = TRUE);
        }
        if (cal.CF != 1) {
            CF <- cal.CF;
            return(parms <<- data.frame(
                Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                    "Alpha2M_Level"),
                Value = c(
                    0,
                    CF * A,
                    CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    CF * v,
                    0),
                ## StdErr = rep(NA, 5),
                Units = c("min", "nM * min", "nM", "min", "nM / min", "nM"))
                   );
        } else {
            return(parms <<- data.frame(
                Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                    "Alpha2M_Level"),
                Value = c(
                    0,
                    A,
                    A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    v,
                    0),
                ## StdErr = rep(NA, 5),
                Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                    "a.u. / min"))
                   );
        }
    } else {
        warning(">> fit$GammaInt does not exist!");
        return(NULL);
    }
}  ## End of TG.parms_GammaInt
################################################################################

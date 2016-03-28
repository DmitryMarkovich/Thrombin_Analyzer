################################################################################
TG.fit_Gamma <- function(silent = TRUE) {
################################################################################
    if (exists(x = "Gamma", where = fit)) {
        warning(">> No fitting: Gamma fit already exists!");
        return(fit$Gamma);
    } else {
        ft <- NULL; start.list <- list(b = data$y[1], A = 0.5 * num.smry$ampl,
                                       k = 3, theta = num.smry$t.peak / 2);
        n.try <- 1;
        while (is.null(ft) && n.try <= N.tries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + A * pgamma(q = x, shape = k, scale = theta),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min),
                    ## upper = c(b.max, A.max, k.max, theta.max),
                    lower = c(  0,   0,   1,   0),
                    upper = c(Inf, Inf, Inf, Inf),
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
                               k = runif(1, 1, 10), theta = runif(1, 1, 30));
        }
        if (is.null(ft)) {
            warning(">> fit_Gamma resulted in NULL!");
            return(NULL);
        } else {
            fit$Gamma <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$Gamma);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitGamma
################################################################################

################################################################################
TG.get_Gamma <- function() {
    if (exists(x = "Gamma", where = fit)) {
        return(fit$Gamma$cff[[1]] + fit$Gamma$cff[[2]] *
                   pgamma(q = data$x, shape = fit$Gamma$cff[[3]],
                          scale = fit$Gamma$cff[[4]]));
    } else {
        warning(">> fit$Gamma does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of TG_get_Gamma
################################################################################

################################################################################
TG.parms_Gamma <- function(cal.CF) {
    print(">> Call to TG.parms_Gamma");
    if (exists(x = "Gamma", where = fit)) {
        A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
        theta <- fit$Gamma$cff[["theta"]];
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
                Units = c("min", "a.u.", "a.u. / min", "min",
                    "a.u. / min * min", "nM"))
                   );
        }
    } else {
        warning(">> fit$Gamma does not exist!");
        return(NULL);
    }
}  ## End of TG.parms_Gamma
################################################################################

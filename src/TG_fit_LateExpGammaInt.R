################################################################################
TG.fit_LateExpGammaInt <- function(silent = TRUE) {
################################################################################
    if (exists(x = "LateExpGammaInt", where = fit)) {
        warning(">> No fitting: LateExpGammaInt fit already exists!");
    } else {
        if (!exists(x = "GammaInt", where = fit)) {
            fit_GammaInt(silent = TRUE);
        }
        start.list <- list(
            b = fit$GammaInt$cff[["b"]], p1 = fit$GammaInt$cff[["A"]],
            A = runif(1, 0, 2), k = fit$GammaInt$cff[["k"]],
            theta = fit$GammaInt$cff[["theta"]], k.a2m = fit$GammaInt$cff[["k.a2m"]]);

        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= N.tries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + p1 *
                        (1 - exp(-A * (
                            pgamma(q = x, shape = k, scale = theta) +
                                (k.a2m / gamma(k)) * (
                                    gamma(k) * pgamma(q = x, shape = k, scale = theta) * x -
                                        gamma(k + 1) * theta *
                                            pgamma(q = x, shape = k + 1, scale = theta))
                        ))),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min),
                    ## upper = c(b.max, A.max, k.max, theta.max),
                    lower = c(  0,   0,   0,   0,   0,   0),
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
            start.list <- list(b = data$y[1], p1 = runif(1, 0, 1) * num.smry$ampl,
                               A = runif(1, 0, 2), k = runif(1, 1, 10),
                               theta = runif(1, 1, 30), k.a2m = runif(1, 0, 1) * 0.05);
        }
        if (is.null(ft)) {
            warning(">> fit_LateExpGammaInt resulted in NULL!");
        } else {
            fit$LateExpGammaInt <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitLateExpGammaInt
################################################################################

################################################################################
TG.get_LateExpGammaInt <- function() {
    if (exists(x = "LateExpGammaInt", where = fit)) {
        b <- fit$LateExpGammaInt$cff[["b"]]; A <- fit$LateExpGammaInt$cff[["A"]];
        k <- fit$LateExpGammaInt$cff[["k"]]; theta <- fit$LateExpGammaInt$cff[["theta"]];
        k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]]
        return(b + p1 * (1 - exp(-A * (
            pgamma(q = data$x, shape = k, scale = theta) +
                (k.a2m / gamma(k)) * (
                    gamma(k) *
                        pgamma(q = data$x, shape = k, scale = theta) * data$x -
                            gamma(k + 1) * theta *
                                pgamma(q = data$x, shape = k + 1, scale = theta))   
        ))));
    } else {
        warning(">> fit$LateExpGammaInt does not exist!");
    }
}  ## End of TG_get_LateExpGammaInt
################################################################################

################################################################################
TG.parms_LateExpGammaInt <- function(cal.CF) {
    print(">> Call to TG.parms_LateExpGammaInt");
    if (exists(x = "LateExpGammaInt", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
        theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
        k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
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
                    CF * p1 * A,
                    CF * p1 * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    CF * p1 * v,
                    CF * p1 * k.a2m * A
                    ),
                ## StdErr = rep(NA, 5),
                Units = c("min", "nM * min", "nM", "min", "nM / min", "nM"))
                   );
        } else {
            return(parms <<- data.frame(
                Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                    "Alpha2M_Level"),
                Value = c(
                    0,
                    p1 * A,
                    p1 * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                    theta * (k - 1),
                    p1 * v,
                    p1 * k.a2m * A
                    ),
                Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                    "nM"))
                   );
        }
    } else {
        warning(">> fit$LateExpGammaInt does not exist!");
    }
}  ## End of TG.parms_LateExpGammaInt
################################################################################




## ################################################################################
## TG.parms_LateExpGammaInt <- function() {
##     print(">> Call to TG.parms_LateExpGammaInt");
##     if (exists(x = "LateExpGammaInt", where = fit)) {
##         A <- fit$LateExpGammaInt$cff[["A"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
##         k <- fit$LateExpGammaInt$cff[["k"]]; theta <- fit$LateExpGammaInt$cff[["theta"]];
##         k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
##         if (k > 2) {
##             v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
##                 exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
##         } else {
##             v <- max(num.smry$drv2, na.rm = TRUE);
##         }
##         if (cal.CF != 1) {
##             CF <- cal.CF;
##             return(parms <<- data.frame(
##                 Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
##                     "Alpha2M_Level"),
##                 Value = c(
##                     t0,
##                     CF * A,
##                     CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     CF * v,
##                     CF * k.a2m * A
##                     ),
##                 ## StdErr = rep(NA, 5),
##                 Units = c("min", "nM * min", "nM", "min", "nM / min", "nM"))
##                    );
##         } else {
##             return(parms <<- data.frame(
##                 Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
##                     "Alpha2M_Level"),
##                 Value = c(
##                     t0,
##                     A,
##                     A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     v,
##                     k.a2m * A
##                     ),
##                 Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
##                     "nM"))
##                    );
##         }
##     } else {
##         warning(">> fit$LateExpGammaInt does not exist!");
##         return(NULL);
##     }
    
##     if (exists(x = "LateExpGammaInt", where = fit)) {
##         ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
##         return(parms <<- data.frame(
##             Parameter = c("ETP", "Peak", "ttPeak", "Vel Index", "Lagtime"),
##             Value = c(fit$LateExpGammaInt$cff[[1]], fit$LateExpGammaInt$cff[[1]],
##                 fit$LateExpGammaInt$cff[[3]] * (fit$LateExpGammaInt$cff[[2]] - 1),
##                       fit$LateExpGammaInt$cff[[2]], 0),
##             ## StdErr = rep(NA, 5),
##             Units = c("nM * min", "nM", "min", "nM / min", "min"))
##                );
##     } else {

##     }
## }  ## End of TG.parms_LateExpGammaInt
## ################################################################################

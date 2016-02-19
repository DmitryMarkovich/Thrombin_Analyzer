################################################################################
T0Gamma <- function(x, cff) {
    return(cff[["b"]] + cff[["A"]] *
               pgamma(q = x, shape = cff[["k"]], scale = cff[["theta"]]));
}  ## End of Gamma()
################################################################################

################################################################################
Thr <- function(x, cff) {
    return(cff[["A"]] *
               pgamma(q = x, shape = cff[["k"]], scale = cff[["theta"]]));
}  ## End of Thr()
################################################################################

################################################################################
Thromb <- function(x, cff) {
    return(cff[["A"]] *
               dgamma(x = x, shape = cff[["k"]], scale = cff[["theta"]]));
}  ## End of Thromb()
################################################################################

################################################################################
ThrombVel <- function(x, cff) {
    v <- rep(0, length(x));
    v[x != 0] <- cff[["A"]] * exp(-x[x != 0] / cff[["theta"]]) *
        x[x != 0] ^ (cff[["k"]] - 1) *
            ((-1 / cff[["theta"]]) +
                 ((cff[["k"]] - 1) / x[x != 0])) /
                     (gamma(cff[["k"]]) * cff[["theta"]] ^ cff[["k"]]);
    return(v);
}  ## End of ThrombVel
################################################################################

################################################################################
PreFitGamma <- function(data, num.smry = NA, silent = TRUE) {
################################################################################
    ## check if num.smry is supplied, calculate it otherwise
    if (is.na(num.smry[1])) {
        num.smry <- ExploreNumerically(data);
    }
    ## fit polynomial growth to pre-peak part of data
    pre.fit <- NULL; n.try <- 1;
    start.list <- list(b = data[[2]][1], A = 0.5 * num.smry$ampl, k = 3);
    while (is.null(pre.fit) && n.try <= N.tries) {
        ## print(paste0(">> n.try = ", n.try, ", start.list = ")); print(start.list);
        try(expr = {
            pre.fit <- nlsLM(formula = y ~ b + A * x ^ (k - 1),
                             data = data[data[[1]] <= num.smry$t.peak, ],
                             start = start.list, trace = F,
                             lower = c(  0,   0,   1),
                             upper = c(Inf, Inf, Inf)
                             );
        }, silent = F);
        n.try <- n.try + 1;
        start.list <- list(b = data[[2]][1], A = runif(1, 0, 1) * num.smry$ampl,
                           k = runif(n = 1, min = 1, max = 10));
    }
    if (is.null(pre.fit)) {
        message(">> PreFitGamma returning NULL!"); return(NULL);
    } else {
        if (!silent) { print(summary(pre.fit));}
        return(list(b = coef(pre.fit)[[1]], A = coef(pre.fit)[[2]],
                    k = coef(pre.fit)[[3]],
                    theta = num.smry$t.peak / (coef(pre.fit)[[3]] - 1)));
    }
################################################################################
}  ## End of PreFitGamma()
################################################################################

################################################################################
TG.fit_T0Gamma <- function(silent = TRUE) {
################################################################################
    if (exists(x = "T0Gamma", where = fit)) {
        warning(">> No fitting: T0Gamma fit already exists!");
    } else {
        if (exists(x = "Gamma", where = fit)) {
            ## use existing Gamma fit for estimates of all parameters except t0
            start.list <- list(b = fit$Gamma$cff[["b"]], A = fit$Gamma$cff[["A"]],
                               k = fit$Gamma$cff[["k"]], theta = fit$Gamma$cff[["theta"]],
                               t0 = 0);
        } else {
            ## call fit_Gamma for estimate all parameters except t0
            fit_Gamma(silent = TRUE);
            start.list <- list(b = fit$Gamma$cff[["b"]], A = fit$Gamma$cff[["A"]],
                               k = fit$Gamma$cff[["k"]], theta = fit$Gamma$cff[["theta"]],
                               t0 = 0);
        }
        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= N.tries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta),
                    data = data, start = start.list, trace = F,
                    ## lower = c(b.min, A.min, k.min, theta.min, t0.min),
                    ## upper = c(b.max, A.max, k.max, theta.max, t0.max),
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
                               t0 = 0);
        }
        if (is.null(ft)) {
            warning(">> fit_T0Gamma resulted in NULL!");
        } else {
            fit$T0Gamma <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
        }  ## End of if is.null(fit)
    }  ## End of if exists()
################################################################################
}  ## End of TG_fitT0Gamma
################################################################################

################################################################################
TG.get_T0Gamma <- function() {
    if (exists(x = "T0Gamma", where = fit)) {
        return(fit$T0Gamma$cff[[1]] + fit$T0Gamma$cff[[2]] *
                   pgamma(q = data$x - fit$T0Gamma$cff[["t0"]],
                          shape = fit$T0Gamma$cff[[3]],
                          scale = fit$T0Gamma$cff[[4]]));
    } else {
        warning(">> fit$T0Gamma does not exist!");
    }
}  ## End of TG_get_T0Gamma
################################################################################

################################################################################
TG.get_T0Gamma_thrombin_int <- function() {
    if (exists(x = "T0Gamma", where = fit)) {
        return(fit$T0Gamma$cff[[2]] *
                   pgamma(q = data$x - fit$T0Gamma$cff[["t0"]],
                          shape = fit$T0Gamma$cff[[3]],
                          scale = fit$T0Gamma$cff[[4]]));
    } else {
        warning(">> fit$T0Gamma does not exist!");
    }
}  ## End of TG_get_T0Gamma
################################################################################

################################################################################
TG.get_T0Gamma_A2mT_int <- function() {
    if (exists(x = "T0Gamma", where = fit)) {
        return(rep(0, length(data$x)));
    } else {
        warning(">> fit$T0Gamma does not exist!");
    }
}  ## End of TG_get_T0Gamma
################################################################################

################################################################################
TG.parms_T0Gamma <- function() {
    print(">> Call to TG.parms_T0Gamma");
    if (exists(x = "T0Gamma", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        return(parms <<- data.frame(
            Parameter = c("ETP", "Peak", "ttPeak", "Vel Index", "Lagtime"),
            Value = c(fit$T0Gamma$cff[[1]], fit$T0Gamma$cff[[1]],
                fit$T0Gamma$cff[["t0"]] +
                    fit$T0Gamma$cff[[3]] * (fit$T0Gamma$cff[[2]] - 1),
                fit$T0Gamma$cff[[2]], fit$T0Gamma$cff[["t0"]]),
            StdErr = rep(NA, 5),
            Units = c("nM * min", "nM", "min", "nM / min", "min"))
               );
    } else {
        warning(">> fit$T0Gamma does not exist!");
    }
}  ## End of TG.parms_T0Gamma
################################################################################

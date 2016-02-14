################################################################################
Gamma <- function(x, cff) {
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
TG.fit_Gamma <- function(silent = TRUE) {
################################################################################
    if (exists(x = "Gamma", where = fit)) {
        warning(">> No fitting: Gamma fit already exists!");
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
        } else {
            fit$Gamma <<- list(
                cff = coef(ft), smty = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
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
    }
}  ## End of TG_get_Gamma
################################################################################

################################################################################
TG.parms_Gamma <- function() {
    print(">> Call to TG.parms_LM");
    if (exists(x = "Gamma", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        return(parms <<- data.frame(
            Parameter = c("ETP", "Peak", "ttPeak", "Vel Index", "Lagtime"),
            Value = c(fit$Gamma$cff[[1]], fit$Gamma$cff[[1]],
                fit$Gamma$cff[[3]] * (fit$Gamma$cff[[2]] - 1),
                      fit$Gamma$cff[[2]], 0),
            StdErr = rep(NA, 5),
            Units = c("nM * min", "nM", "min", "nM / min", "min"))
               );
    } else {
        warning(">> fit$Gamma does not exist!");
    }
}  ## End of TG.parms_LM
################################################################################

################################################################################
Cal.fit_LateExp <- function(silent = FALSE) {
    if (exists(x = "LateExp", where = fit)) {
        warning(">> No fitting: LateExp fit already exists!");
        return(fit$LateExp);
    } else {
        print(">> fit_LateExp called!");
        if (exists(x = "LM", where = fit)) {
            ## use existing LM fit for estimate of p3
            start.list <- list(b = data$y[[1]], p1 = num.smry$ampl,
                               p3 = fit$LM$cff[[2]] / num.smry$ampl);
        } else {
            ## call fitLM and use cff[[2]] as estimate for p3
            fit_LM(silent = TRUE);
            start.list <- list(b = data$y[[1]], p1 = num.smry$ampl,
                               p3 = fit$LM$cff[[2]]);
        }
        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= N.tries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + p1 * (1 - exp(-p3 * x)),
                    data = data, start = start.list, algorithm = "LM",
                    lower = c(0, 0, 0),
                    upper = c(Inf, Inf, Inf),
                    control = nls.lm.control(
                        ftol = sqrt(.Machine$double.eps),
                        ptol = sqrt(.Machine$double.eps),
                        gtol = 0, nprint = -1, factor = 100,  ## between [0.1, 100]
                        maxiter = 200
                    )
                )
            }, silent = F);
            n.try <- n.try + 1;
            start.list <- list(b = data$y[1], p1 = runif(1, 0, 2) * num.smry$ampl,
                               p3 = runif(1, 0, 2) * fit$LM$cff[[2]] / num.smry$ampl);
        }
        if (is.null(ft)) {
            warning(">> Cal.fit_LateExp resulted in NULL");
            return(NULL);
        } else {
            fit$LateExp <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$LateExp);
        }  ## End of if (is.null(ft))
    }  ## End of if (exists())
}  ## End of Cal.fit_LateExp
################################################################################

################################################################################
Cal.get_LateExp <- function() {
    if (exists(x = "LateExp", where = fit)) {
        return(
            fit$LateExp$cff[[1]] + fit$LateExp$cff[[2]] *
                (1 - exp(-fit$LateExp$cff[[3]] * data$x))
        );
    } else {
        warning(">> fit$LateExp does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal.get_LateExp
################################################################################

################################################################################
Cal.get_init_rate_LateExp <- function() {
    print(">> Call to Cal.parms_LateExp");
    if (exists(x = "LateExp", where = fit)) {
        b <- fit$LateExp$cff[["b"]];
        p1 <- fit$LateExp$cff[["p1"]]; p3 <- fit$LateExp$cff[["p3"]];
        return(b + (p1 * p3) * data$x);
    } else {
        warning(">> fit$LateExp does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal.get_init_rate_LateExp
################################################################################

################################################################################
Cal.parms_LateExp <- function(e0, s0) {
    if (exists(x = "LateExp", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        p1 <- fit$LateExp$cff[["p1"]]; p3 <- fit$LateExp$cff[["p3"]];
        return(parms <<- data.frame(
            Parameter = c("e0", "s0", "CF_CAT", "TC_Initial_Slope"),
            Value = c(e0, s0, e0 / (p1 * p3), p1 * p3),
            ## StdErr = rep(NA, 3),
            Units = c("nM", "uM", "nM * min / a.u.", "a.u. / min"))
               );
    } else {
        warning(">> fit$LateExp does not exist!");
        return(NULL);
    }
}  ## End of Cal.parms_LateExp
################################################################################

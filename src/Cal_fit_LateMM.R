################################################################################
Cal.fit_LateMM <- function(silent = TRUE) {
    if (exists(x = "LateMM", where = fit)) {
        warning(">> No fitting: LateMM fit already exists!");
        return(fit$LateMM);
    } else {
        print(">> fit_LateMM called!");
        if (exists(x = "LateExp", where = fit)) {
            ## use existing LateExp fit for estimate of p1 and p3
            start.list <- list(b = data[[2]][1], p1 = fit$LateExp$cff[["p1"]],
                               p2 = 1, p3 = fit$LateExp$cff[["p3"]]);
        } else {
            ## call fit_LateExp for estimate of p1 and p3
            fit_LateExp(silent = TRUE);
            start.list <- list(b = data[[2]][1], p1 = fit$LateExp$cff[["p1"]],
                               p2 = 1, p3 = fit$LateExp$cff[["p3"]]);
        }
        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= kNumTries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + p1 * (1 - (W(p2 * exp(p2) * exp(-p3 * x)) / p2)),
                    data = data, start = start.list, algorithm = "LM",
                    lower = c(  0,   0,   0,   0),
                    upper = c(Inf, Inf, Inf, Inf),
                    control = nls.lm.control(
                        ftol = 0.1 * sqrt(.Machine$double.eps),
                        ptol = 0.1 * sqrt(.Machine$double.eps),
                        gtol = 0, nprint = -1, factor = 100,  ## between [0.1, 100]
                        maxiter = 200
                    )
                )
            }, silent = F);
            n.try <- n.try + 1;
            start.list <- list(b = data$y[1],
                               p1 = runif(1) * num.smry$ampl,
                               p2 = runif(1, 0, 2),
                               p3 = runif(1) * fit$LateExp$cff[["p3"]]);
        }
        if (is.null(ft)) {
            warning(">> Cal.fit_LateMM resulted in NULL");
            return(NULL);
        } else {
            fit$LateMM <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$LateMM);
        }  ## End of if (is.null(ft))
    }  ## End of if (exists())
}  ## End of Cal.fit_LateMM
################################################################################

################################################################################
Cal.get_LateMM <- function() {
    if (exists(x = "LateMM", where = fit)) {
        ## y ~ b + p1 * (1 - (W(p2 * exp(p2) * exp(-p3 * x)) / p2)),
        b <- fit$LateMM$cff[["b"]]; p1 <- fit$LateMM$cff[["p1"]];
        p2 <- fit$LateMM$cff[["p2"]]; p3 <- fit$LateMM$cff[["p3"]];
        return(b + p1 * (1 - (W(p2 * exp(p2) * exp(-p3 * data$x)) / p2)));
    } else {
        warning(">> fit$LateMM does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal.get_LateMM
################################################################################

##         e0 = e0, s0 = s0, C = cff[["p1"]] / s0, K.m = s0 / cff[["p2"]],
##         k.cat = (s0 / e0) * (cff[["p3"]] / cff[["p2"]])

################################################################################
Cal.parms_LateMM <- function(e0, s0) {
    print(">> Call to Cal.parms_LateMM");
    if (exists(x = "LateMM", where = fit)) {
        ## print(fit$LateMM);
        p1 <- fit$LateMM$cff[["p1"]]; p2 <- fit$LateMM$cff[["p2"]];
        p3 <- fit$LateMM$cff[["p3"]];
        ## print(paste0(p1, p2, p3));
        ## print(paste0(e0, s0));
        Value <- c(e0, s0, e0 * (1 + p2) / (p1 * p3), e0 / (p1 * p3),
                   1e3 * s0 / p2, (1e3 * s0 / e0) * (p3 / p2), p1 / (1e3 * s0),
                   (p1 * p3 / (1 + p2)));
        print(Value);
        ## print(parms);  ##str(fit$LateMM);

        return(parms <<- data.frame(
            Parameter = c("e0", "s0", "CF_CAT", "CF_DTU", "K.m", "k.cat", "I",
                "TC_Initial_Slope"),
            Value = Value,
            ## StdErr = rep(NA, 7),
            Units = c("nM", "uM", "nM * min / a.u.",
                "nM * min / a.u.", "nM", "nM / min",
                "a.u. / nM", "a.u. / min"))
               );
    } else {
        warning(">> fit$LateMM does not exist!");
        return(NULL);
    }
}  ## End of Cal.parms_LateMM
################################################################################

################################################################################
Cal.get_init_rate_LateMM <- function() {
    print(">> Call to Cal.parms_LateMM");
    if (exists(x = "LateMM", where = fit)) {
        p1 <- fit$LateMM$cff[["p1"]]; p2 <- fit$LateMM$cff[["p2"]];
        p3 <- fit$LateMM$cff[["p3"]]; b <- fit$LateMM$cff[["b"]];
        return(b + (p1 * p3 / (1 + p2)) * data$x);
    } else {
        warning(">> fit$LateMM does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal.get_init_rate_LateMM
################################################################################

## ################################################################################
## LateMM <- function(x, cff) {
##     return(cff[["b"]] + cff[["p1"]] *
##                (1 - (W(cff[["p2"]] * exp(cff[["p2"]]) *
##                            exp(-cff[["p3"]] * x)) / cff[["p2"]])));
## }  ## End of LateMM
## ################################################################################

## ################################################################################
## LateMMLinearPart <- function(x, cff) {
##     ## calculates only the linear part of the product concentration ob
##     ## Late-times MM kinetics solution
##     ## time - time vector in minutes
##     ## late.cff - coefficients for Late-times MM formula obtained from fit
##     return(cff[["b"]] + (cff[["p1"]] * cff[["p3"]] / (1 + cff[["p2"]])) * x);
## }  ## End of LateMMLinearPart
## ################################################################################

## ################################################################################
## LateMMCFCAT <- function(cff, e0 = 100) {
##     return(e0 * (1 + cff[["p2"]]) / (cff[["p1"]] * cff[["p3"]]));
## }  ## End of LateMMCF
## ################################################################################

## ################################################################################
## LateMMCFDTU <- function(cff, e0 = 100) {
##     return(e0 / (cff[["p1"]] * cff[["p3"]]));
## }  ## End of LateMMCFDTU
## ################################################################################

## ################################################################################
## LateMMKinConst <- function(cff, e0 = 100, s0 = 454000) {
##     return(list(
##         e0 = e0, s0 = s0, C = cff[["p1"]] / s0, K.m = s0 / cff[["p2"]],
##         k.cat = (s0 / e0) * (cff[["p3"]] / cff[["p2"]])
##     ));
## }  ## End of LateMMKinConst
## ################################################################################

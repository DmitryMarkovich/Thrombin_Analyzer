################################################################################
Cal.fit_EarlyMM <- function(silent = FALSE) {
    if (exists(x = "EarlyMM", where = fit)) {
        warning(">> No fitting: EarlyMM fit already exists!");
        return(fit$EarlyMM);
    } else {
        print(">> fit_EarlyMM called!");
        if (exists(x = "LM", where = fit)) {
            ## use existing LM fit for estimate of p1
            start.list <- list(b = data$y[[1]], p1 = fit$LM$cff[[2]], p2 = 0.1);
        } else {
            ## call fitLM and use cff[[2]] as estimate for p1
            fit_LM(silent = TRUE);
            start.list <- list(b = data$y[[1]], p1 = fit$LM$cff[[2]], p2 = 0.1);
        }
        ft <- NULL; n.try <- 1;
        while (is.null(ft) && n.try <= kNumTries) {
            try(expr = {
                ft <- nlsLM(
                    y ~ b + p1 * (x - ((1 - exp(-p2 * x)) / p2)),
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
            start.list <- list(b = data$y[1], p1 = runif(1) * fit$LM$cff[[2]],
                               p2 = runif(1));
        }
        if (is.null(ft)) {
            warning(">> Cal.fit_EarlyMM resulted in NULL");
            return(NULL);
        } else {
            fit$EarlyMM <<- list(
                cff = coef(ft), smry = summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
            );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$EarlyMM);
        }  ## End of if (is.null(ft))
    }  ## End of if (exists())
}  ## End of Cal.fit_EarlyMM
################################################################################

################################################################################
Cal.get_EarlyMM <- function() {
    if (exists(x = "EarlyMM", where = fit)) {
        b <- fit$EarlyMM$cff[[1]]; p1 <- fit$EarlyMM$cff[[2]];
        p2 <- fit$EarlyMM$cff[[3]];
        ## (x - ((1 - exp(-p2 * x)) / p2))
        return(b + p1 * (data$x - ((1 - exp(-p2 * data$x)) / p2)));
    } else {
        warning(">> fit$EarlyMM does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal.get_EarlyMM
################################################################################

################################################################################
Cal.parms_EarlyMM <- function(e0, s0) {
    if (exists(x = "EarlyMM", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        return(parms <<- data.frame(
            Parameter = c("e0", "s0", "CF_CAT", "TC_Initial_Slope"),
            Value = c(e0, s0, e0 / fit$EarlyMM$cff[["p1"]], fit$EarlyMM$cff[["p1"]]),
            ## StdErr = rep(NA, 3),
            Units = c("nM", "uM", "nM * min / a.u.", "a.u. / min"))
               );
    } else {
        warning(">> fit$EarlyMM does not exist!");
        return(NULL);
    }
}  ## End of Cal.parms_EarlyMM
################################################################################

## EarlyMM <- function(time, early.cff) {
##     p.b <- early.cff[[1]]; p1 <- early.cff[[2]]; p2 <- early.cff[[3]];
##     early.cff[[2]] *
##         (time - ((1 - exp(-early.cff[[3]] * time)) / early.cff[[3]]));
## return(
##     p.b + p1 *
##         (time - ((1 - exp(-p2 * time)) / p2))
## );
## }  ## End of EarlyMM

## EarlyMMLinearPart <- function(time, early.cff) {
##     ## time - time vector in minutes
##     ## late.cff - coefficients for EarlyMM formula obtained from fit
##     p.b <- early.cff[[1]]; p1 <- early.cff[[2]]; p2 <- early.cff[[3]];
##     return(p.b + p1 * time - (p1 / p2));
## }  ## End of EarlyMMLinearPart

## EarlyMMCF <- function(early.cff, e0) {
##     p1 <- early.cff[[2]];
##     return(e0 / p1);
## }  ## End of EarlyMMCF

################################################################################
Cal.fit_LM <- function(silent = TRUE) {
    if (exists(x = "LM", where = fit)) {
        warning(">> No fitting: LM fit already exists!");
    } else {
        print(">> fit_LM called!");
        ft <- lm(y ~ x, data = data);
        fit$LM <<- list(
            cff = coef(ft), smry = summary(ft),
            diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
        );
        if (!silent)
            print(fit);
        return(fit$LM);
        ## parms_LM();
    }  ## End of if (exists)
}  ## End of Cal.fit_LM
################################################################################

################################################################################
Cal.get_LM <- function() {
    if (exists(x = "LM", where = fit)) {
        return(fit$LM$cff[[1]] + fit$LM$cff[[2]] * data$x);
    } else {
        warning(">> fit$LM does not exist!");
    }
}  ## End of Cal.get_LM
################################################################################

################################################################################
Cal.parms_LM <- function() {
    print(">> Call to Cal.parms_LM");
    if (exists(x = "LM", where = fit)) {
        ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
        return(parms <<- data.frame(
            Parameter = c("e0", "s0", "CF_CAT"),
            Value = c(e0, s0, e0 / fit$LM$cff[[2]]),
            StdErr = rep(NA, 3), Units = c("nM", "uM", "nM * min / a.u."))
               );
    } else {
        warning(">> fit$LM does not exist!");
    }
}  ## End of Cal.parms_LM
################################################################################

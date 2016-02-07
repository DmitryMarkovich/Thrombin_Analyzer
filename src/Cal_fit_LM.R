################################################################################
Cal.fit_LM <- function(silent = FALSE) {
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

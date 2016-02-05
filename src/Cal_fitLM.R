################################################################################
Cal.fitLM <- function(calModel) {
    if (any(calModel == names(fit))) {
        warning(">> LM fit already exists!");
    } else {
        print(">> fitLM called!");
        ft <- lm(y ~ x, data = data);
        fit$LM <<- list(
            cff = coef(ft), smry = summary(ft),
            diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
        );
        ## print(fit);
    }
}  ## End of Cal.fitLM
################################################################################

################################################################################
Cal.getLM <- function() {
    if (exists(x = "LM", where = fit)) {
        return(fit$LM$cff[[1]] + fit$LM$cff[[2]] * data$x);
    } else {
        warning(">> fit$LM does not exist!");
    }
}  ## End of Cal.getLM
################################################################################

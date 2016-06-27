################################################################################
Cal$set(
    which = "public", name = "fit_LM",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (exists(x = "LM", where = fit)) {
                warning(">> No fitting: LM fit already exists!");
                return(fit$LM);
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
            }  ## End of if (exists)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$fit_LM
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_LM",
    value = compiler::cmpfun(
        f = function() {
            if (exists(x = "LM", where = fit)) {
                return(fit$LM$cff[[1]] + fit$LM$cff[[2]] * data$x);
            } else {
                warning(">> fit$LM does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_LM
################################################################################

################################################################################
Cal$set(
    which = "public", name = "parms_LM",
    value = compiler::cmpfun(
        f = function(e0, s0) {
            ## print(">> Call to Cal.parms_LM");
            if (exists(x = "LM", where = fit)) {
                return(parms <<- data.frame(
                    Parameter = kParameterNamesLM,
                    Value = c(e0, s0, e0 / fit$LM$cff[[2]],
                        fit$LM$cff[[2]]),
                    ## StdErr = rep(NA, 3),
                    Units = kParameterUnitsLM
                    ));
            } else {
                warning(">> fit$LM does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$parms_LM
################################################################################

################################################################################
######################################## Legacy RF classes code
################################################################################

## ################################################################################
## Cal.fit_LM <- function(silent = TRUE) {
##     if (exists(x = "LM", where = fit)) {
##         warning(">> No fitting: LM fit already exists!");
##         return(fit$LM);
##     } else {
##         print(">> fit_LM called!");
##         ft <- lm(y ~ x, data = data);
##         fit$LM <<- list(
##             cff = coef(ft), smry = summary(ft),
##             diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
##         );
##         if (!silent)
##             print(fit);
##         return(fit$LM);
##     }  ## End of if (exists)
## }  ## End of Cal.fit_LM
## ################################################################################

## ################################################################################
## Cal.get_LM <- function() {
##     if (exists(x = "LM", where = fit)) {
##         return(fit$LM$cff[[1]] + fit$LM$cff[[2]] * data$x);
##     } else {
##         warning(">> fit$LM does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of Cal.get_LM
## ################################################################################

## ################################################################################
## Cal.parms_LM <- function(e0, s0) {
##     ## print(">> Call to Cal.parms_LM");
##     if (exists(x = "LM", where = fit)) {
##         return(parms <<- data.frame(
##             Parameter = kParameterUnitsLM,
##             Value = c(e0, s0, e0 / fit$LM$cff[[2]], fit$LM$cff[[2]]),
##             ## StdErr = rep(NA, 3),
##             Units = kParameterUnitsLM)
##                );
##     } else {
##         warning(">> fit$LM does not exist!");
##         return(NULL);
##     }
## }  ## End of Cal.parms_LM
## ################################################################################

################################################################################
######################################## End of Legacy RF classes code
################################################################################

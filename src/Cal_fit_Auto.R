################################################################################
Cal$set(
    which = "public", name = "fit_Auto",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (!is.null(fit$Auto)) {
                ## print(fit$Auto);
                warning(">> No fitting: Auto fit already exists!");
            } else {
                print(">>> Cal.fit_Auto called!");
                ft <- NULL;
                if (num.smry$rat$x <= 1.5 && num.smry$rat$y <= 6) {
                    ft <- fit_EarlyMM(silent = TRUE);
                    if (!is.null(ft)) {
                        fit$Auto <<- TRUE;
                        fit$Auto_model <<- "EarlyMM";
                    } else {
                        fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                    }
                } else if (num.smry$rat$x >= 5 && num.smry$rat$x <= 25 &&
                           num.smry$rat$y >= 10 && num.smry$rat$y <= 30) {
                    ft <- fit_LateExp(silent = TRUE);
                    if (!is.null(ft)) {
                        ft2 <- fit_T0LateExp(silent = TRUE);
                        compare_two_models("LateExp", "T0LateExp", ft, ft2);
                        ## fit$Auto <<- ft;
                        ## fit$Auto_model <<- "T0LateExp";
                    } else {
                        fit$Auto <<- FALSE; fit$Auto_model <<- "None"; 
                    }
                } else if (num.smry$rat$x >= 15 && num.smry$rat$y >= 40) {
                    ft <- fit_LateMM(silent = TRUE);
                    if (!is.null(ft)) {
                        ft2 <- fit_T0LateMM(silent = TRUE);
                        compare_two_models("LateMM", "T0LateMM", ft, ft2);
                        ## fit$Auto <<- ft;
                        ## fit$Auto_model <<- "T0LateMM";
                    } else {
                        fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                    }
                } else {
                    fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                }
            }  ## End of if (exists)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$fit_Auto
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_Auto",
    value = compiler::cmpfun(
        f = function() {
            if (exists(x = "Auto", where = fit)) {
                return(get_model(fit$Auto_model));
            } else {
                warning(">> fit$Auto does not exist!");
                return(rep(NA, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_Auto
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_init_rate_Auto",
    value = compiler::cmpfun(
        f = function() {
            if (exists(x = "Auto", where = fit)) {
                return(get_init_rate(fit$Auto_model));
            } else {
                warning(">> fit$Auto does not exist!");
                return(rep(NA, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_init_rate_Auto
################################################################################

################################################################################
Cal$set(
    which = "public", name = "parms_Auto",
    value = compiler::cmpfun(
        f = function(e0, s0) {
            print(">> Call to Cal.parms_Auto");
            if (exists(x = "Auto", where = fit)) {
                return(parms_model(fit$Auto_model, e0, s0));
            } else {
                warning(">> fit$Auto does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);
################################################################################

################################################################################
######################################## Legacy RF classes code
################################################################################

## ################################################################################
## Cal.fit_Auto <- function(silent = TRUE) {
##     if (exists(x = "Auto", where = fit)) {
##         print(fit$Auto);
##         warning(">> No fitting: Auto fit already exists!");
##     } else {
##         print(">>> Cal.fit_Auto called!");
##         ft <- NULL;
##         if (num.smry$rat$x <= 1.5 && num.smry$rat$y <= 6) {
##             ft <- fit_EarlyMM(silent = TRUE);
##             if (!is.null(ft)) {
##                 fit$Auto <<- TRUE;
##                 fit$Auto_model <<- "EarlyMM";
##             } else {
##                 fit$Auto_model <<- "None";
##             }
##         } else if (num.smry$rat$x >= 5 && num.smry$rat$x <= 25 &&
##                    num.smry$rat$y >= 10 && num.smry$rat$y <= 30) {
##             ft <- fit_T0LateExp(silent = TRUE);
##             if (!is.null(ft)) {
##                 fit$Auto <<- ft;
##                 fit$Auto_model <<- "T0LateExp";
##             } else {
##                 fit$Auto_model <<- "None";
##             }
##         } else if (num.smry$rat$x >= 15 && num.smry$rat$y >= 40) {
##             ft <- fit_T0LateMM(silent = TRUE);
##             if (!is.null(ft)) {
##                 fit$Auto <<- ft;
##                 fit$Auto_model <<- "T0LateMM";
##             } else {
##                 fit$Auto_model <<- "None";
##             }
##         }
##     }  ## End of if (exists)
## }  ## End of Cal.fit_Auto
## ################################################################################

## ################################################################################
## Cal.get_Auto <- function() {
##     if (exists(x = "Auto", where = fit)) {
##         return(get_model(fit$Auto_model));
##     } else {
##         warning(">> fit$Auto does not exist!");
##         return(rep(NA, length(data$x)));
##     }
## }  ## End of Cal_get_Auto
## ################################################################################

## ################################################################################
## Cal.get_init_rate_Auto <- function() {
##     if (exists(x = "Auto", where = fit)) {
##         return(get_init_rate(fit$Auto_model));
##     } else {
##         warning(">> fit$Auto does not exist!");
##         return(rep(NA, length(data$x)));
##     }
## }  ## End of Cal_get_init_rate_Auto
## ################################################################################

## ################################################################################
## Cal.parms_Auto <- function(e0, s0) {
##     print(">> Call to Cal.parms_Auto");
##     if (exists(x = "Auto", where = fit)) {
##         return(parms_model(fit$Auto_model, e0, s0));
##     } else {
##         warning(">> fit$Auto does not exist!");
##         return(NULL);
##     }
## }  ## End of Cal.parms_Auto
## ################################################################################

################################################################################
######################################## End of Legacy RF classes code
################################################################################

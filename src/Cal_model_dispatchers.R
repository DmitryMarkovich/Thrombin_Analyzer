################################################################################
Cal$set(
    which = "public", name = "fit_model",
    value = compiler::cmpfun(
        f = function(cal.model) {
            switch(cal.model,
                   "LM" = fit_LM(silent = TRUE),
                   "EarlyMM" = fit_EarlyMM(silent = TRUE),
                   "LateExp" = fit_LateExp(silent = TRUE),
                   "T0LateExp" = fit_T0LateExp(silent = TRUE),
                   "LateMM" = fit_LateMM(silent = TRUE),
                   "T0LateMM" = fit_T0LateMM(silent = TRUE),
                   "Auto" = fit_Auto(silent = TRUE),
                   {
                       warning(paste0(">> Call to unknown model", cal.model));
                       return(NULL);
                   }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$fit_model
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_model",
    value = compiler::cmpfun(
        f = function(cal.model) {
            switch(cal.model,
                   "LM" = get_LM(),
                   "EarlyMM" = get_EarlyMM(),
                   "LateExp" = get_LateExp(),
                   "T0LateExp" = get_T0LateExp(),
                   "LateMM" = get_LateMM(),
                   "T0LateMM" = get_T0LateMM(),
                   "Auto" = get_Auto(),
                   {
                       warning(paste0(">> Call to unknown get_model",
                                      cal.model));
                       return(NULL);
                   }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_model
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_init_rate",
    value = compiler::cmpfun(
        f = function(cal.model) {
            switch(cal.model,
                   "LateExp" = get_init_rate_LateExp(),
                   "T0LateExp" = get_init_rate_LateExp(),
                   "LateMM" = get_init_rate_LateMM(),
                   "T0LateMM" = get_init_rate_T0LateMM(),
                   "Auto" = get_init_rate_Auto(),
                   {
                       warning(paste0(">> Call to unknown get_model",
                                      cal.model));
                       return(rep(0, length(data$x)));
                   }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_init_rate
################################################################################

################################################################################
Cal$set(
    which = "public", name = "parms_model",
    value = compiler::cmpfun(
        f = function(cal.model, e0, s0) {
            switch(cal.model,
                   "LM" = parms_LM(e0, s0),
                   "EarlyMM" = parms_EarlyMM(e0, s0),
                   "LateExp" = parms_LateExp(e0, s0),
                   "T0LateExp" = parms_LateExp(e0, s0),
                   "LateMM" = parms_LateMM(e0, s0),
                   "T0LateMM" = parms_T0LateMM(e0, s0),
                   "Auto" = parms_Auto(e0, s0),
                   {
                       warning(paste0(">> Call to unknown model", cal.model));
                       return(NULL);
                   }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$parms_model
################################################################################

################################################################################
######################################## Legacy RF classes code
################################################################################

## ################################################################################
## Cal.fit_model <- function(cal.model) {
##     switch(cal.model,
##            "LM" = fit_LM(silent = TRUE),
##            "EarlyMM" = fit_EarlyMM(silent = TRUE),
##            "LateExp" = fit_LateExp(silent = TRUE),
##            "T0LateExp" = fit_T0LateExp(silent = TRUE),
##            "LateMM" = fit_LateMM(silent = TRUE),
##            "T0LateMM" = fit_T0LateMM(silent = TRUE),
##            "Auto" = fit_Auto(silent = TRUE),
##            {
##                warning(paste0(">> Call to unknown model", cal.model));
##                return(NULL);
##            }
##            );
## }  ## End of Cal.fit_model
## ################################################################################

## ################################################################################
## Cal.get_model <- function(cal.model) {
##     switch(cal.model,
##            "LM" = get_LM(),
##            "EarlyMM" = get_EarlyMM(),
##            "LateExp" = get_LateExp(),
##            "T0LateExp" = get_T0LateExp(),
##            "LateMM" = get_LateMM(),
##            "T0LateMM" = get_T0LateMM(),
##            "Auto" = get_Auto(),
##            {
##                warning(paste0(">> Call to unknown get_model", cal.model));
##                return(NULL);
##            }
##            );
## }  ## End of Cal.get_model
## ################################################################################

## ################################################################################
## Cal.get_init_rate <- function(cal.model) {
##     switch(cal.model,
##            "LateExp" = get_init_rate_LateExp(),
##            "T0LateExp" = get_init_rate_LateExp(),
##            "LateMM" = get_init_rate_LateMM(),
##            "T0LateMM" = get_init_rate_T0LateMM(),
##            "Auto" = get_init_rate_Auto(),
##            {
##                warning(paste0(">> Call to unknown get_model", cal.model));
##                return(rep(0, length(data$x)));
##            }
##            );
## }  ## End of Cal.get_model
## ################################################################################

## ################################################################################
## Cal.parms_model <- function(cal.model, e0, s0) {
##     switch(cal.model,
##            "LM" = parms_LM(e0, s0),
##            "EarlyMM" = parms_EarlyMM(e0, s0),
##            "LateExp" = parms_LateExp(e0, s0),
##            "T0LateExp" = parms_LateExp(e0, s0),
##            "LateMM" = parms_LateMM(e0, s0),
##            "T0LateMM" = parms_T0LateMM(e0, s0),
##            "Auto" = parms_Auto(e0, s0),
##            {
##                warning(paste0(">> Call to unknown model", cal.model));
##                return(NULL);
##            }
##            );
## }  ## End of Cal.parms_model
## ################################################################################

################################################################################
######################################## End of Legacy RF classes code
################################################################################

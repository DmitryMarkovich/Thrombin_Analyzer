################################################################################
TG$set(
    which = "public", name = "fit_model",
    value = compiler::cmpfun(
        f = function(tg.model, silent = TRUE) {
            ## print(tg.model);
            switch(tg.model,
                   ## "Gamma" = fit_Gamma(silent = TRUE),
                   "T0Gamma" = fit_T0Gamma(silent = silent),
                   ## "GammaInt" = fit_GammaInt(silent = silent),
                   "T0GammaInt" = fit_T0GammaInt(silent = silent),
                   "T0GammaInt2" = fit_T0GammaInt2(silent = silent),
                   ## "LateExpGammaInt" = fit_LateExpGammaInt(silent = silent),
                   "LateExpT0GammaInt" = fit_LateExpT0GammaInt(silent = silent),
                   "Auto" = fit_Auto(silent = silent),
                   { warning(paste0(">> Call to unknown model", tg.model))}
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$fit_model
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_model",
    value = compiler::cmpfun(
        f = function(tg.model) {
            switch(tg.model,
                   "None" = return(rep(NA, length(data$x))),
                   ## "Gamma" = get_Gamma(),
                   "T0Gamma" = get_T0Gamma(),
                   ## "GammaInt" = get_GammaInt(),
                   "T0GammaInt" = get_T0GammaInt(),
                   "T0GammaInt2" = get_T0GammaInt2(),
                   ## "LateExpGammaInt" = get_LateExpGammaInt(),
                   "LateExpT0GammaInt" = get_LateExpT0GammaInt(),
                   "Auto" = get_Auto(),
                   { warning(paste0(">> Call to unknown get_model ", tg.model))}
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_model
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_model",
    value = compiler::cmpfun(
        f = function(tg.model, cal.CF = 1) {
            switch(tg.model,
                   "None" = return(parms <<- data.frame(
                       Parameter = kParameterNames,
                       Value = rep(NA, 6), Units = kAUnits)),
                   ## "Gamma" = parms_Gamma(cal.CF),
                   "T0Gamma" = parms_T0Gamma(cal.CF),
                   ## "GammaInt" = parms_GammaInt(cal.CF),
                   "T0GammaInt" = parms_T0GammaInt(cal.CF),
                   "T0GammaInt2" = parms_T0GammaInt2(cal.CF),
                   ## "LateExpGammaInt" = parms_LateExpGammaInt(cal.CF),
                   "LateExpT0GammaInt" = parms_LateExpT0GammaInt(cal.CF),
                   "Auto" = parms_Auto(cal.CF),
                   { warning(paste0(">> Call to unknown model ", tg.model))}
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$parms_model
################################################################################

################################################################################
TG$set(
    which = "public", name = "fit_Auto",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (!is.null(fit$Auto)) {  ## exists(x = "Auto", where = fit, envir = environment()) 
                print(">> No fitting: Auto fit already exists!");
            } else {
                ft <- NULL;  ## print(num.smry);
                if (num.smry$rat$y <= kYNone) {
                    ## print(">> Chose None model");
                    fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                    ## print(">> None model:");
                    ## print(fit$Auto); print(fit$Auto_model);
                } else if (num.smry$rat$x <= kXT0GammaInt &&
                   num.smry$rat$y <= kYT0GammaInt) {
                    if (num.smry$rat$x <= kXT0Gamma) {
                        ## print(">> Chose T0Gamma model");
                        ft <- fit_T0Gamma(silent = TRUE);
                        if (!is.null(ft)) {
                            fit$Auto <<- TRUE; fit$Auto_model <<- "T0Gamma";
                        } else {
                            fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                        }
                    } else {
                        ## print(">> Chose T0GammaInt model");
                        ft <- fit_T0Gamma(silent = TRUE);
                        if (!is.null(ft)) {
                            ft2 <- fit_T0GammaInt(silent = TRUE);
                            compare_two_models("T0GammaInt", "T0Gamma", ft2, ft);
                            ## compare_T0GammaInt_and_T0Gamma(ft, ft2);
                            ## fit$Auto <<- TRUE; fit$Auto_model <<- "T0GammaInt";
                        } else {
                            fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                        }
                    }
                } else {
                    ## print(">> Chose T0GammaInt model");
                    ft <- fit_T0GammaInt(silent = TRUE);  ## print(ft);
                    if (!is.null(ft)) {
                        if (max(data$x) >= kTimeSC) {
                            ft.sc <- fit_LateExpT0GammaInt(silent = TRUE);
                        } else {
                            ft.sc <- NULL;
                        }
                        if (!is.null(ft.sc) &&
                            need_substrate_consumption(ft, ft.sc)) {
                            ## ft.sc was successful and sc is required
                            fit[["T0GammaInt"]] <<- NULL;
                            ## print(">> fit_Auto: switching to sc models.");
                            if (kMode == "Accuracy") {
                                ft2 <- fit_LateExpT0GammaInt2(silent = TRUE);
                            } else {
                                ft2 <- NULL;
                            }
                            compare_two_models("LateExpT0GammaInt",
                                               "LateExpT0GammaInt2", ft, ft2);
                        } else {
                            fit[["LateExpT0GammaInt"]] <<- NULL;
                            ## print(">> fit_Auto: staying with no sc models.");
                            if (kMode == "Accuracy") {
                                ft2 <- fit_T0GammaInt2(silent = TRUE);
                            } else {
                                ft2 <- NULL;
                            }
                            compare_two_models("T0GammaInt",
                                               "T0GammaInt2", ft, ft2);
                        }
                    } else {
                        fit$Auto <<- FALSE; fit$Auto_model <<- "None";
                    }
                    ## print(fit);
                }
            }  ## End of if (exists)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$fit_Auto
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_Auto",
    value = compiler::cmpfun(
        f = function() {
            if (exists(x = "Auto", where = fit)) {
                return(get_model(fit$Auto_model));
            } else {
                print(">> fit$Auto does not exist!");
                return(rep(NA, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_Auto
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_Auto",
    value = compiler::cmpfun(
        f = function(cal.CF) {
            ## print(">> Call to TG.parms_Auto");
            ## if (exists(x = "Auto", where = fit, envir = environment())) {
            if (!is.null(fit$Auto)) {
                return(parms_model(fit$Auto_model, cal.CF));
            } else {
                print(">> fit$Auto does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$parms_Auto
################################################################################

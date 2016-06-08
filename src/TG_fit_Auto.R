################################################################################
TG.compare_T0GammaInt2_and_T0GammaInt <- function(ft, ft2, silent = TRUE) {
    if (is.null(ft2)) {
        if (!silent) {
            warning(">> T0GammaInt2 does not exist!");
            warning(">> Returning T0GammaInt without comparison!");
        }
        fit$Auto <<- ft; fit$Auto_model <<- "T0GammaInt";
        return(0L);
    } else {
        if (ft$smry$sigma <= ft2$smry$sigma) {
            if (!silent)
                print(">> Returning T0GammaInt because of lower sigma!");
            fit$Auto <<- ft; fit$Auto_model <<- "T0GammaInt";
            return(0L);
        } else {
            if (!silent)
                print(">> Returning T0GammaInt2 because of lower sigma!");
            fit$Auto <<- ft2; fit$Auto_model <<- "T0GammaInt2";
            return(0L);
        }
    }  ## End of if()
}  ## End of TG.compare_T0GammaInt2_and_T0GammaInt
################################################################################

################################################################################
TG.fit_Auto <- function(silent = TRUE) {
    if (exists(x = "Auto", where = fit)) {
        warning(">> No fitting: Auto fit already exists!");
    } else {
        ft <- NULL;
        if (num.smry$rat$y <= 3) {
            fit$Auto_model <<- "None";
        } else if (num.smry$rat$x <= 2.5 && num.smry$rat$y <= 34) {
            if (num.smry$rat$x <= 1.6) {
                ft <- fit_T0Gamma(silent = TRUE);
                if (!is.null(ft)) {
                    fit$Auto <<- ft;
                    fit$Auto_model <<- "T0Gamma";
                } else {
                    fit$Auto_model <<- "None";
                }
            } else {
                ft <- fit_T0GammaInt(silent = TRUE);
                if (!is.null(ft)) {
                    fit$Auto <<- ft;
                    fit$Auto_model <<- "T0GammaInt";
                } else {
                    fit$Auto_model <<- "None";
                }
            }
        } else {
            ft <- fit_T0GammaInt(silent = TRUE);  ## print(ft);
            if (!is.null(ft)) {
                ft2 <- fit_T0GammaInt2(silent = TRUE);  ## print(ft2);
                compare_T0GammaInt2_and_T0GammaInt(ft, ft2);
            } else {
                fit$Auto_model <<- "None";
            }
        }
    }  ## End of if (exists)
}  ## End of TG.fit_Auto
################################################################################

################################################################################
TG.get_Auto <- function() {
    if (exists(x = "Auto", where = fit)) {
        return(get_model(fit$Auto_model));
    } else {
        warning(">> fit$Auto does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of TG_get_Auto
################################################################################

################################################################################
TG.parms_Auto <- function(cal.CF) {
    print(">> Call to TG.parms_Auto");
    if (exists(x = "Auto", where = fit)) {
        return(parms_model(fit$Auto_model, cal.CF));
    } else {
        warning(">> fit$Auto does not exist!");
        return(NULL);
    }
}  ## End of TG.parms_Auto
################################################################################

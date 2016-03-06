################################################################################
TG.fit_Auto <- function(silent = TRUE) {
    if (exists(x = "Auto", where = fit)) {
        warning(">> No fitting: Auto fit already exists!");
    } else {
        ft <- NULL;
        if (num.smry$rat$x <= 2 && num.smry$rat$y <= 20) {
            ft <- fit_T0Gamma(silent = TRUE);
            if (!is.null(ft)) {
                fit$Auto <<- ft;
                fit$Auto_model <<- "T0Gamma";
            }
        } else {
            ft <- fit_T0GammaInt(silent = TRUE);
            if (!is.null(ft)) {
                fit$Auto <<- ft;
                fit$Auto_model <<- "T0GammaInt";
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

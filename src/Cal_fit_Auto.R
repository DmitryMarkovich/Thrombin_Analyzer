################################################################################
Cal.fit_Auto <- function(silent = TRUE) {
    if (exists(x = "Auto", where = fit)) {
        print(fit$Auto);
        warning(">> No fitting: Auto fit already exists!");
    } else {
        print(">>> Cal.fit_Auto called!");
        ft <- NULL;
        if (num.smry$rat$x <= 1.5 && num.smry$rat$y <= 6) {
            ft <- fit_EarlyMM(silent = TRUE);
            if (!is.null(ft)) {
                fit$Auto <<- ft;
                fit$Auto_model <<- "EarlyMM";
            }
        } else if (num.smry$rat$x >= 15 && num.smry$rat$y >= 40) {
            ft <- fit_LateMM(silent = TRUE);
            if (!is.null(ft)) {
                fit$Auto <<- ft;
                fit$Auto_model <<- "LateMM";
            }
        }
    }  ## End of if (exists)
}  ## End of Cal.fit_Auto
################################################################################

################################################################################
Cal.get_Auto <- function() {
    if (exists(x = "Auto", where = fit)) {
        return(get_model(fit$Auto_model));
    } else {
        warning(">> fit$Auto does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal_get_Auto
################################################################################

################################################################################
Cal.get_init_rate_Auto <- function() {
    if (exists(x = "Auto", where = fit)) {
        return(get_init_rate(fit$Auto_model));
    } else {
        warning(">> fit$Auto does not exist!");
        return(rep(0, length(data$x)));
    }
}  ## End of Cal_get_init_rate_Auto
################################################################################

################################################################################
Cal.parms_Auto <- function(e0, s0) {
    print(">> Call to Cal.parms_Auto");
    if (exists(x = "Auto", where = fit)) {
        return(parms_model(fit$Auto_model, e0, s0));
    } else {
        warning(">> fit$Auto does not exist!");
        return(NULL);
    }
}  ## End of Cal.parms_Auto
################################################################################

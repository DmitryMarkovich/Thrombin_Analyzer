################################################################################
TG$set(
    which = "public", name = "get_thrombin_int",
    value = compiler::cmpfun(
        f = function(tg.model) {
            switch(tg.model,
                   "None" = {
                       return(rep(NA, length(data$x)));
                   },
                   "Gamma" = {
                       if (!is.null(fit$Gamma)) {  ## exists(x = "Gamma", where = fit)
                           A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
                           theta <- fit$Gamma$cff[["theta"]];
                           return(A * pgamma(q = data$x, shape = k, scale = theta));
                       } else {
                           warning(">> fit$Gamma does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0Gamma" = {
                       if (!is.null(fit$T0Gamma)) {  ## exists(x = "T0Gamma", where = fit)
                           A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                           theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
                           return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
                       } else {
                           warning(">> fit$T0Gamma does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "GammaInt" = {
                       if (!is.null(fit$GammaInt)) {  ## exists(x = "GammaInt", where = fit)
                           A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                           theta <- fit$GammaInt$cff[["theta"]];
                           return(A * pgamma(q = data$x, shape = k, scale = theta));
                       } else {
                           warning(">> fit$GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt" = {
                       if (!is.null(fit$T0GammaInt)) {  ## exists(x = "T0GammaInt", where = fit)
                           A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                           theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                           return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
                       } else {
                           warning(">> fit$T0GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt2" = {
                       if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                           return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
                                      A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
                       } else {
                           warning(">> fit$T0GammaInt2 does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpGammaInt" = {
                       if (!is.null(fit$LateExpGammaInt)) {  ## exists(x = "LateExpGammaInt", where = fit)
                           A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                           theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                           ## return(p1 * (1 - exp(-A * pgamma(q = data$x, shape = k, scale = theta))));
                           return(p1 * A * pgamma(q = data$x, shape = k, scale = theta));
                       } else {
                           warning(">> fit$LateExpGammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt" = {
                       if (!is.null(fit$LateExpT0GammaInt)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                           A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                           ## return(p1 * (1 - exp(-A * pgamma(q = data$x - t0, shape = k, scale = theta))));
                           return(p1 * A * pgamma(q = data$x - t0, shape = k, scale = theta))
                       } else {
                           warning(">> fit$LateExpT0GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (!is.null(fit$Auto)) {  ##exists(x = "Auto", where = fit)
                           return(get_thrombin_int(fit$Auto_model));
                       } else {
                           warning(">> fit$Auto does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   { print(paste0(">> Call to unknown tg.model", tg.model))}
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_int
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin_int_contribution",
    value = compiler::cmpfun(
        f = function(tg.model, number = 1) {
            if (tg.model == "T0GammaInt2") {
                if (!is.null(fit$T0GammaInt)) {  ## exists(x = "T0GammaInt2", where = fit)
                    A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                    k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                    theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                    if (number == 1) {
                        return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta));
                    } else if (number == 2) {
                        return(A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
                    }
                }
            }
            return(rep(NA, length(data$x)));
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_int_contribution
################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.get_thrombin_int <- function(tg.model) {
##     switch(tg.model,
##            "None" = {
##                return(rep(NA, length(data$x)));
##            },
##            "Gamma" = {
##                if (exists(x = "Gamma", where = fit)) {
##                    A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
##                    theta <- fit$Gamma$cff[["theta"]];
##                    return(A * pgamma(q = data$x, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$Gamma does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "T0Gamma" = {
##                if (exists(x = "T0Gamma", where = fit)) {
##                    A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
##                    theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
##                    return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$T0Gamma does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "GammaInt" = {
##                if (exists(x = "GammaInt", where = fit)) {
##                    A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
##                    theta <- fit$GammaInt$cff[["theta"]];
##                    return(A * pgamma(q = data$x, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$GammaInt does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "T0GammaInt" = {
##                if (exists(x = "T0GammaInt", where = fit)) {
##                    A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
##                    theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
##                    return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$T0GammaInt does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "T0GammaInt2" = {
##                if (exists(x = "T0GammaInt2", where = fit)) {
##                    A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
##                    k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
##                    theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
##                    return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
##                               A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
##                } else {
##                    warning(">> fit$T0GammaInt2 does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "LateExpGammaInt" = {
##                if (exists(x = "LateExpGammaInt", where = fit)) {
##                    A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
##                    theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
##                    ## return(p1 * (1 - exp(-A * pgamma(q = data$x, shape = k, scale = theta))));
##                    return(p1 * A * pgamma(q = data$x, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$LateExpGammaInt does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "LateExpT0GammaInt" = {
##                if (exists(x = "LateExpT0GammaInt", where = fit)) {
##                    A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
##                    theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
##                    t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
##                    ## return(p1 * (1 - exp(-A * pgamma(q = data$x - t0, shape = k, scale = theta))));
##                    return(p1 * A * pgamma(q = data$x - t0, shape = k, scale = theta))
##                } else {
##                    warning(">> fit$LateExpT0GammaInt does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            "Auto" = {
##                if (exists(x = "Auto", where = fit)) {
##                    return(get_thrombin_int(fit$Auto_model));
##                } else {
##                    warning(">> fit$Auto does not exist!");
##                    return(rep(NA, length(data$x)));
##                }
##            },
##            { print(paste0(">> Call to unknown tg.model", tg.model))}
##            );
## }  ## End of TG.get_thrombin_int
## ################################################################################

## ################################################################################
## TG.get_thrombin_int_contribution <- function(tg.model, number = 1) {
##     if (tg.model == "T0GammaInt2") {
##         if (exists(x = "T0GammaInt2", where = fit)) {
##             A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
##             k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
##             theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
##             if (number == 1) {
##                 return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta));
##             } else if (number == 2) {
##                 return(A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
##             }
##         }
##     }
##     return(rep(NA, length(data$x)));
## }  ## End of TG.get_thrombin_int_contribution
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

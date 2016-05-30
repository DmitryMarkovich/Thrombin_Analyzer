################################################################################
TG.get_thrombin <- function(tg.model, time = NULL) {
    switch(tg.model,
           "Gamma" = {
               if (exists(x = "Gamma", where = fit)) {
                   A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
                   theta <- fit$Gamma$cff[["theta"]];
                   return(A * dgamma(x = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$Gamma does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0Gamma" = {
               if (exists(x = "T0Gamma", where = fit)) {
                   A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                   theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
                   return(A * dgamma(x = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$T0Gamma does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "GammaInt" = {
               if (exists(x = "GammaInt", where = fit)) {
                   A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                   theta <- fit$GammaInt$cff[["theta"]];
                   return(A * dgamma(x = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0GammaInt" = {
               if (exists(x = "T0GammaInt", where = fit)) {
                   A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                   theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                   return(A * dgamma(x = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0GammaInt2" = {
               if (exists(x = "T0GammaInt2", where = fit)) {
                   A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                   k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                   theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                   if (!is.null(time)) {
                       return(A1 * dgamma(x = time - t0, shape = k1, scale = theta) +
                                  A2 * dgamma(x = time - t0, shape = k2, scale = theta));
                   } else {
                       return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta) +
                                  A2 * dgamma(x = data$x - t0, shape = k2, scale = theta));
                   }
               } else {
                   warning(">> fit$T0GammaInt2 does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpGammaInt" = {
               if (exists(x = "LateExpGammaInt", where = fit)) {
                   A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                   theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                   k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                   ## return(p1 * exp(-(A * pgamma(q = data$x, shape = k, scale = theta) +
                   ##                       (A * k.a2m / gamma(k)) * (
                   ##                           gamma(k) *
                   ##                               pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
                   ##                                   gamma(k + 1) * theta *
                   ##                                       pgamma(q = data$x, shape = k + 1, scale = theta))
                   ##                 )) * A * dgamma(x = data$x, shape = k, scale = theta)
                   ##        );
                   return(p1 * A * dgamma(x = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$LateExpGammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpT0GammaInt" = {
               if (exists(x = "LateExpT0GammaInt", where = fit)) {
                   A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                   theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                   t0 <- fit$LateExpT0GammaInt$cff[["t0"]]; k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                   ## return(p1 * exp(-(A * pgamma(q = data$x - t0, shape = k, scale = theta) +
                   ##                       (A * k.a2m / gamma(k)) * (
                   ##                           gamma(k) *
                   ##                               pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                   ##                                   gamma(k + 1) * theta *
                   ##                                       pgamma(q = data$x - t0, shape = k + 1, scale = theta))
                   ##                   )) * A * dgamma(x = data$x - t0, shape = k, scale = theta)
                   ##        );
                   return(p1 * A * dgamma(x = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$LateExpT0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "Auto" = {
               if (exists(x = "Auto", where = fit)) {
                   return(get_thrombin(fit$Auto_model));
               } else {
                   warning(">> fit$Auto does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           {  ## Default
               warning(paste0(">> Call to unknown tg.model ", tg.model));
               return(rep(0, length(data$x)));
           }
           );  ## End of switch(tg.model)
}  ## End of TG.get_thrombin
################################################################################

################################################################################
TG.get_thrombin_contribution <- function(tg.model, number = 1, time = NULL) {
    if (tg.model == "T0GammaInt2") {
        if (exists(x = "T0GammaInt2", where = fit)) {
            A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
            k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
            theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
            if (number == 1) {
                if (!is.null(time)) {
                    return(A1 * dgamma(x = time - t0, shape = k1, scale = theta));
                } else {
                    return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta));
                }
            } else if (number == 2) {
                if (!is.null(time)) {
                    return(A2 * dgamma(x = time - t0, shape = k2, scale = theta));
                } else {
                    return(A2 * dgamma(x = data$x - t0, shape = k2, scale = theta));
                }
            }
        }
    }
    return(rep(0, length(data$x)));
}  ## End of TG.get_thrombin_contribution
################################################################################

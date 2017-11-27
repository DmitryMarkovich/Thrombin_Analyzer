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
                   "T0GammaInt2_test" = {
                       if (!is.null(fit$T0GammaInt2_test)) {  ## exists(x = "T0GammaInt2_test", where = fit)
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta1) +
                                  A2 * pgamma(q = data$x - t0, shape = k2, scale = theta2));
                       } else {
                           warning(">> fit$T0GammaInt2_test does not exist!");
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
                   "LateExpT0GammaInt2" = {
                       if (!is.null(fit$LateExpT0GammaInt2)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                           p1 <- fit$LateExpT0GammaInt2$cff[["p1"]];
                           A1 <- fit$LateExpT0GammaInt2$cff[["A1"]];
                           k1 <- fit$LateExpT0GammaInt2$cff[["k1"]];
                           A2 <- fit$LateExpT0GammaInt2$cff[["A2"]];
                           k2 <- fit$LateExpT0GammaInt2$cff[["k2"]];
                           theta <- fit$LateExpT0GammaInt2$cff[["theta"]];
                           t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];
                           return(p1 * (
                               A1 * pgamma(q = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * pgamma(q = data$x - t0, shape = k2,
                                           scale = theta)
                           ));
                       } else {
                           warning(">> fit$LateExpT0GammaInt2 does not exist!");
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
                   { print(paste0(">> In get_thrombin_int: call to unknown tg.model",
                                  tg.model));
                       return(rep(NA, length(data$x)));
                   }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_int
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin_int_contribution",
    value = compiler::cmpfun(
        f = function(tg.model, number = 1) {
            switch(tg.model,
                   "T0GammaInt2" = {
                       if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                           if (number == 1) {
                               return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta));
                           } else if (number == 2) {
                               return(A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
                           }
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (!is.null(fit$T0GammaInt2_test)) {  ## exists(x = "T0GammaInt2_test", where = fit)
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           if (number == 1) {
                               return(A1 * pgamma(q = data$x - t0, shape = k1, scale = theta1));
                           } else if (number == 2) {
                               return(A2 * pgamma(q = data$x - t0, shape = k2, scale = theta2));
                           }
                       }
                   },
                   "LateExpT0GammaInt2" = {
                       if (!is.null(fit$LateExpT0GammaInt2)) {  ## exists(x = "LateExpT0GammaInt2", where = fit)
                           p1 <- fit$LateExpT0GammaInt2$cff[["p1"]];
                           A1 <- fit$LateExpT0GammaInt2$cff[["A1"]];
                           A2 <- fit$LateExpT0GammaInt2$cff[["A2"]];
                           k1 <- fit$LateExpT0GammaInt2$cff[["k1"]];
                           k2 <- fit$LateExpT0GammaInt2$cff[["k2"]];
                           theta <- fit$LateExpT0GammaInt2$cff[["theta"]];
                           t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];
                           if (number == 1) {
                               return(p1 * A1 * pgamma(q = data$x - t0, shape = k1, scale = theta));
                           } else if (number == 2) {
                               return(p1 * A2 * pgamma(q = data$x - t0, shape = k2, scale = theta));
                           }
                       }
                   }
            );
            return(rep(NA, length(data$x)));
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_int_contribution
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin_vel",
    value = compiler::cmpfun(
        f = function(tg.model, time = NULL) {
            switch(tg.model,
                   "None" = {
                       return(rep(NA, length(data$x)));
                   },
                   "Gamma" = {
                       if (exists(x = "Gamma", where = fit)) {
                           A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
                           theta <- fit$Gamma$cff[["theta"]];
                           v <- A * exp(-data$x / theta) * data$x ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / data$x)) /
                                   (gamma(k) * theta ^ k);
                           v[data$x == 0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$Gamma does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0Gamma" = {
                       if (exists(x = "T0Gamma", where = fit)) {
                           A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                           theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
                           v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                                   (gamma(k) * theta ^ k);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$T0Gamma does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "GammaInt" = {
                       if (exists(x = "GammaInt", where = fit)) {
                           A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                           theta <- fit$GammaInt$cff[["theta"]];
                           v <- A * exp(-data$x / theta) * data$x ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / data$x)) /
                                   (gamma(k) * theta ^ k);
                           v[data$x == 0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt" = {
                       if (exists(x = "T0GammaInt", where = fit)) {
                           A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                           theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                           v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                                   (gamma(k) * theta ^ k);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$T0GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt2" = {
                       if (exists(x = "T0GammaInt2", where = fit)) {
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                           if (!is.null(time)) {
                               v <- A1 * exp(-(time - t0) / theta) * (time - t0) ^ (k1 - 1) *
                                   ((-1 / theta) + ((k1 - 1) / (time - t0))) /
                                       (gamma(k1) * theta ^ k1) +
                                           A2 * exp(-(time - t0) / theta) * (time - t0) ^ (k2 - 1) *
                                               ((-1 / theta) + ((k2 - 1) / (time - t0))) /
                                                   (gamma(k2) * theta ^ k2);
                               v[time <= t0] <- 0;
                               return(v);
                           } else {
                               v <- A1 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k1 - 1) *
                                   ((-1 / theta) + ((k1 - 1) / (data$x - t0))) /
                               (gamma(k1) * theta ^ k1) +
                                   A2 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k2 - 1) *
                                       ((-1 / theta) + ((k2 - 1) / (data$x - t0))) /
                                           (gamma(k2) * theta ^ k2);
                               v[data$x <= t0] <- 0;
                               return(v);
                           }
                       } else {
                           warning(">> fit$T0GammaInt2 does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (exists(x = "T0GammaInt2_test", where = fit)) {
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           if (!is.null(time)) {
                               v <- A1 * exp(-(time - t0) / theta1) * (time - t0) ^ (k1 - 1) *
                                   ((-1 / theta1) + ((k1 - 1) / (time - t0))) /
                                       (gamma(k1) * theta1 ^ k1) +
                                           A2 * exp(-(time - t0) / theta2) * (time - t0) ^ (k2 - 1) *
                                               ((-1 / theta2) + ((k2 - 1) / (time - t0))) /
                                                   (gamma(k2) * theta2 ^ k2);
                               v[time <= t0] <- 0;
                               return(v);
                           } else {
                               v <- A1 * exp(-(data$x - t0) / theta1) * (data$x - t0) ^ (k1 - 1) *
                                   ((-1 / theta1) + ((k1 - 1) / (data$x - t0))) /
                               (gamma(k1) * theta1 ^ k1) +
                                   A2 * exp(-(data$x - t0) / theta2) * (data$x - t0) ^ (k2 - 1) *
                                       ((-1 / theta2) + ((k2 - 1) / (data$x - t0))) /
                                           (gamma(k2) * theta2 ^ k2);
                               v[data$x <= t0] <- 0;
                               return(v);
                           }
                       } else {
                           warning(">> fit$T0GammaInt2_test does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpGammaInt" = {
                       if (exists(x = "LateExpGammaInt", where = fit)) {
                           A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                           theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                           v <- p1 * A * exp(-(data$x) / theta) * (data$x) ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / (data$x))) /
                                   (gamma(k) * theta ^ k);
                           v[data$x <= 0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$LateExpGammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt" = {
                       if (exists(x = "LateExpT0GammaInt", where = fit)) {
                           A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                           v <- p1 * A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                                   (gamma(k) * theta ^ k);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$LateExpT0GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt2" = {
                       if (exists(x = "LateExpT0GammaInt2", where = fit)) {
                           p1 <- fit$LateExpT0GammaInt2$cff[["p1"]];
                           A1 <- fit$LateExpT0GammaInt2$cff[["A1"]];
                           k1 <- fit$LateExpT0GammaInt2$cff[["k1"]];
                           A2 <- fit$LateExpT0GammaInt2$cff[["A2"]];
                           k2 <- fit$LateExpT0GammaInt2$cff[["k2"]];
                           theta <- fit$LateExpT0GammaInt2$cff[["theta"]];
                           t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];
                           v <- p1 * (
                               A1 * exp(-(data$x - t0) / theta) * (data$x - t0) ^
                               (k1 - 1) *
                               ((-1 / theta) + ((k1 - 1) / (data$x - t0))) /
                               (gamma(k1) * theta ^ k1) +
                               A2 * exp(-(data$x - t0) / theta) * (data$x - t0) ^
                               (k2 - 1) *
                               ((-1 / theta) + ((k2 - 1) / (data$x - t0))) /
                               (gamma(k2) * theta ^ k2)
                           );
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$LateExpT0GammaInt2 does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (exists(x = "Auto", where = fit)) {
                           return(get_thrombin_vel(fit$Auto_model));
                       } else {
                           warning(">> fit$Auto does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   {  ## Default
                       warning(paste0(">> Call to unknown tg.model ", tg.model));
                       return(rep(NA, length(data$x)));
                   }
                   );  ## End of switch(tg.model)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_vel
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin_vel_contribution",
    value = compiler::cmpfun(
        f = function(tg.model, number = 1, time = NULL) {
            switch(tg.model,
                   "T0GammaInt2" = {
                       if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
                           A1 <- fit$T0GammaInt2$cff[["A1"]];
                           A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]];
                           k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]];
                           t0 <- fit$T0GammaInt2$cff[["t0"]];
                           if (number == 1) {
                               if (!is.null(time)) {
                                   return(get_velocity(time, t0, A1, k1, theta));
                               } else {
                                   return(get_velocity(data$x, t0, A1, k1, theta));
                               }
                           } else if (number == 2) {
                               if (!is.null(time)) {
                                   return(get_velocity(time, t0, A2, k2, theta));
                               } else {
                                   return(get_velocity(data$x, t0, A2, k2, theta));
                               }
                           }
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (!is.null(fit$T0GammaInt2_test)) {  ## exists(x = "T0GammaInt2_test", where = fit)
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]];
                           A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]];
                           k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]];
                           theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           if (number == 1) {
                               if (!is.null(time)) {
                                   return(get_velocity(time, t0, A1, k1, theta1));
                               } else {
                                   return(get_velocity(data$x, t0, A1, k1, theta1));
                               }
                           } else if (number == 2) {
                               if (!is.null(time)) {
                                   return(get_velocity(time, t0, A2, k2, theta2));
                               } else {
                                   return(get_velocity(data$x, t0, A2, k2, theta2));
                               }
                           }
                       }
                   },
                   "LateExpT0GammaInt2" = {
                       p1 <- fit$LateExpT0GammaInt2$cff[["p1"]];
                       A1 <- fit$LateExpT0GammaInt2$cff[["A1"]];
                       A2 <- fit$LateExpT0GammaInt2$cff[["A2"]];
                       k1 <- fit$LateExpT0GammaInt2$cff[["k1"]];
                       k2 <- fit$LateExpT0GammaInt2$cff[["k2"]];
                       theta <- fit$LateExpT0GammaInt2$cff[["theta"]];
                       t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];
                       if (!is.null(time)) {
                           return(p1 * get_velocity(time = time,
                                                    t0 = t0,
                                                    A = get(paste0("A", number)),
                                                    k = get(paste0("k", number)),
                                                    theta = theta)
                                  );
                       } else {
                           return(p1 * get_velocity(time = data$x,
                                                    t0 = t0,
                                                    A = get(paste0("A", number)),
                                                    k = get(paste0("k", number)),
                                                    theta = theta)
                                  );
                       }
                   }
                   );
            return(rep(NA, length(data$x)));
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_vel_contribution
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_drv2",
    value = compiler::cmpfun(
        f = function(tg.model) {
            switch(tg.model,
                   "None" = {
                       return(rep(0, length(data$x)));
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
                           return(rep(0, length(data$x)));
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
                           return(rep(0, length(data$x)));
                       }
                   },
                   "GammaInt" = {
                       if (exists(x = "GammaInt", where = fit)) {
                           A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                           theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
                           v <- A * exp(-data$x / theta) * data$x ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / data$x)) /
                                   (gamma(k) * theta ^ k) +
                                       k.a2m * A * dgamma(x = data$x, shape = k,
                                                          scale = theta);
                           v[data$x == 0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$GammaInt does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "T0GammaInt" = {
                       if (exists(x = "T0GammaInt", where = fit)) {
                           A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                           theta <- fit$T0GammaInt$cff[["theta"]]; k.a2m <- fit$T0GammaInt$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt$cff[["t0"]];
                           v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                               (((-1 / theta) + ((k - 1) / (data$x - t0))) /
                                    (gamma(k) * theta ^ k)) +
                                   k.a2m * A * dgamma(x = data$x - t0, shape = k,
                                                      scale = theta);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$T0GammaInt does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "T0GammaInt2" = {
                       if (exists(x = "T0GammaInt2", where = fit)) {
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]];
                           k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt2$cff[["t0"]];
                           v <- A1 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k1 - 1) *
                               (((-1 / theta) + ((k1 - 1) / (data$x - t0))) /
                                    (gamma(k1) * theta ^ k1)) +
                                   k.a2m * A1 * dgamma(x = data$x - t0, shape = k1,
                                                       scale = theta) +
                                       A2 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k2 - 1) *
                                           (((-1 / theta) + ((k2 - 1) / (data$x - t0))) /
                                                (gamma(k2) * theta ^ k2)) +
                                               k.a2m * A2 * dgamma(x = data$x - t0, shape = k2,
                                                                   scale = theta);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$T0GammaInt2 does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (exists(x = "T0GammaInt2_test", where = fit)) {
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];                           
                           k.a2m <- fit$T0GammaInt2_test$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           v <- A1 * exp(-(data$x - t0) / theta1) * (data$x - t0) ^ (k1 - 1) *
                               (((-1 / theta1) + ((k1 - 1) / (data$x - t0))) /
                                    (gamma(k1) * theta1 ^ k1)) +
                                   k.a2m * A1 * dgamma(x = data$x - t0, shape = k1,
                                                       scale = theta1) +
                                       A2 * exp(-(data$x - t0) / theta2) * (data$x - t0) ^ (k2 - 1) *
                                           (((-1 / theta2) + ((k2 - 1) / (data$x - t0))) /
                                                (gamma(k2) * theta2 ^ k2)) +
                                               k.a2m * A2 * dgamma(x = data$x - t0, shape = k2,
                                                                   scale = theta2);
                           v[data$x <= t0] <- 0;
                           return(v);
                       } else {
                           warning(">> fit$T0GammaInt2_test does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "LateExpGammaInt" = {
                       if (exists(x = "LateExpGammaInt", where = fit)) {
                           p1 <- fit$LateExpGammaInt$cff[["p1"]];
                           A <- fit$LateExpGammaInt$cff[["A"]];
                           k <- fit$LateExpGammaInt$cff[["k"]];
                           theta <- fit$LateExpGammaInt$cff[["theta"]];
                           k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                           T <- A * pgamma(q = data$x, shape = k, scale = theta);
                           A2 <- (A * k.a2m / gamma(k)) * (gamma(k) * pgamma(
                               q = data$x, shape = k, scale = theta) * (data$x) -
                                   gamma(k + 1) * theta * pgamma(
                                       q = data$x, shape = k + 1, scale = theta));
                           T1 <- A * dgamma(x = data$x, shape = k, scale = theta);
                           A21 <- k.a2m * A * pgamma(q = data$x, shape = k, scale = theta);
                           T2 <- A * exp(-(data$x) / theta) * (data$x) ^ (k - 1) *
                               ((-1 / theta) + ((k - 1) / (data$x))) /
                                   (gamma(k) * theta ^ k);
                           T2[data$x <= 0] <- 0;
                           A22 <- k.a2m * A * dgamma(x = data$x, shape = k, scale = theta);
                           return(p1 * exp(-(T + A2)) * (T2 + A22 - (T1 + A21) ^ 2));
                       } else {
                           warning(">> fit$LateExpGammaInt does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt" = {
                       if (exists(x = "LateExpT0GammaInt", where = fit)) {
                           p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           A <- fit$LateExpT0GammaInt$cff[["A"]];
                           k <- fit$LateExpT0GammaInt$cff[["k"]];
                           theta <- fit$LateExpT0GammaInt$cff[["theta"]];
                           k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                           t0 <- fit$LateExpT0GammaInt$cff[["t0"]];

                           T <- A * pgamma(q = data$x - t0, shape = k, scale = theta);
                           ## print(summary(T));
                           A2mT <- (A * k.a2m / gamma(k)) *
                               (gamma(k) * pgamma(q = data$x - t0, shape = k,
                                                  scale = theta) *
                                (data$x - t0) -
                                gamma(k + 1) * theta * pgamma(q = data$x - t0,
                                                              shape = k + 1,
                                                              scale = theta));
                           ## print(summary(A2));
                           T1 <- A * dgamma(x = data$x - t0, shape = k,
                                            scale = theta);
                           ## print(summary(T1));
                           A2mT1 <- k.a2m * A * pgamma(q = data$x - t0,
                                                       shape = k, scale = theta);
                           ## print(summary(A21));
                           T2 <- A * exp(-(data$x - t0) / theta) *
                               (data$x - t0) ^ (k - 1) *
                                                   ((-1 / theta) + ((k - 1) /
                                                                    (data$x - t0))) /
                                                   (gamma(k) * theta ^ k);
                           T2[data$x <= t0] <- 0;
                           ## print(summary(T2));
                           A2mT2 <- k.a2m * A * dgamma(x = data$x - t0,
                                                       shape = k, scale = theta);
                           ## print(summary(A22));
                           ## print(summary(p1 * exp(-(T + A2)) * (T2 + A22 - (T1 + A21) ^ 2)));
                           return(p1 * exp(-(T + A2mT)) *
                                  (T2 + A2mT2 - (T1 + A2mT1) ^ 2));
                           ## A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           ## theta <- fit$LateExpT0GammaInt$cff[["theta"]]; k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                           ## t0 <- fit$LateExpT0GammaInt$cff[["t0"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           ## print(c(A, k, theta, k.a2m, t0, p1));
                           ## T <- A * pgamma(q = data$x - t0, shape = k, scale = theta);
                           ## str(T);
                           ## A <- (A * k.a2m / gamma(k)) * (gamma(k) * pgamma(
                           ##     q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                           ##         gamma(k + 1) * theta * pgamma(
                           ##             q = data$x - t0, shape = k + 1, scale = theta));
                           ## str(A);
                           ## T1 <- A * dgamma(x = data$x - t0, shape = k, scale = theta);
                           ## A1 <- k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta);
                           ## T2 <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                           ##     ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           ##         (gamma(k) * theta ^ k);
                           ## T2[data$x <= t0] <- 0;
                           ## A2 <- k.a2m * A * dgamma(x = data$x - t0, shape = k, scale = theta);
                           ## str(p1 * T2);
                           ## str(A2);
                           ## ## v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                           ## ##     ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           ## ##         (gamma(k) * theta ^ k) +
                           ## ##             k.a2m * A * dgamma(x = data$x - t0, shape = k,
                           ## ##                                scale = theta);
                           ## ## v[data$x <= t0] <- 0;
                           ## return(
                           ##     p1 * (T2 + A2)
                           ##     ## p1 * exp(-(T + A)) * (T2 + A2 - 1 * (T1 + A1)^ 2)
                           ##     );
                           ## return(rep(0, length(data$x)));
                       } else {
                           warning(">> fit$LateExpT0GammaInt does not exist!");
                           return(rep(0, length(data$x)));
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
                           k.a2m <- fit$LateExpT0GammaInt2$cff[["k.a2m"]];
                           t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];

                           T <- (
                               A1 * pgamma(q = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * pgamma(q = data$x - t0, shape = k2,
                                           scale = theta)
                           );
                           ## print(summary(T));
                           A2mT <- (
                               (A1 * k.a2m / gamma(k1)) * (
                                   gamma(k1) *
                                   pgamma(q = data$x - t0,
                                          shape = k1, scale = theta) *
                                   (data$x - t0) -
                                   gamma(k1 + 1) *
                                   theta * pgamma(q = data$x - t0, shape = k1 + 1,
                                                  scale = theta)) +
                               (A2 * k.a2m / gamma(k2)) * (
                                   gamma(k2) *
                                   pgamma(q = data$x - t0,
                                          shape = k2, scale = theta) *
                                   (data$x - t0) -
                                   gamma(k2 + 1) *
                                   theta * pgamma(q = data$x - t0, shape = k2 + 1,
                                                  scale = theta))
                           );
                           ## print(summary(A2));
                           T1 <- (
                               A1 * dgamma(x = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * dgamma(x = data$x - t0, shape = k2,
                                           scale = theta)
                           );
                           ## print(summary(T1));
                           A2mT1 <- k.a2m * (
                               A1 * pgamma(q = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * pgamma(q = data$x - t0, shape = k2,
                                           scale = theta)
                           );
                           ## print(summary(A21));
                           T2 <- (
                               A1 * exp(-(data$x - t0) / theta) * (data$x - t0) ^
                               (k1 - 1) *
                               ((-1 / theta) + ((k1 - 1) / (data$x - t0))) /
                               (gamma(k1) * theta ^ k1) + A2 *
                               exp(-(data$x - t0) / theta) * (data$x - t0) ^
                               (k2 - 1) *
                               ((-1 / theta) + ((k2 - 1) / (data$x - t0))) /
                               (gamma(k2) * theta ^ k2)
                           );
                           T2[data$x <= t0] <- 0;
                           ## print(summary(T2));
                           A2mT2 <- k.a2m * (
                               A1 * dgamma(x = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * dgamma(x = data$x - t0, shape = k2,
                                           scale = theta)
                           );
                           ## print(summary(A22));
                           ## print(summary(p1 * exp(-(T + A2)) * (T2 + A22 - (T1 + A21) ^ 2)));
                           return(p1 * exp(-(T + A2mT)) *
                                  (T2 + A2mT2 - (T1 + A2mT1) ^ 2));
                           ## A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           ## theta <- fit$LateExpT0GammaInt$cff[["theta"]]; k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                           ## t0 <- fit$LateExpT0GammaInt$cff[["t0"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           ## print(c(A, k, theta, k.a2m, t0, p1));
                           ## T <- A * pgamma(q = data$x - t0, shape = k, scale = theta);
                           ## str(T);
                           ## A <- (A * k.a2m / gamma(k)) * (gamma(k) * pgamma(
                           ##     q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                           ##         gamma(k + 1) * theta * pgamma(
                           ##             q = data$x - t0, shape = k + 1, scale = theta));
                           ## str(A);
                           ## T1 <- A * dgamma(x = data$x - t0, shape = k, scale = theta);
                           ## A1 <- k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta);
                           ## T2 <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                           ##     ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           ##         (gamma(k) * theta ^ k);
                           ## T2[data$x <= t0] <- 0;
                           ## A2 <- k.a2m * A * dgamma(x = data$x - t0, shape = k, scale = theta);
                           ## str(p1 * T2);
                           ## str(A2);
                           ## ## v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                           ## ##     ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           ## ##         (gamma(k) * theta ^ k) +
                           ## ##             k.a2m * A * dgamma(x = data$x - t0, shape = k,
                           ## ##                                scale = theta);
                           ## ## v[data$x <= t0] <- 0;
                           ## return(
                           ##     p1 * (T2 + A2)
                           ##     ## p1 * exp(-(T + A)) * (T2 + A2 - 1 * (T1 + A1)^ 2)
                           ##     );
                           ## return(rep(0, length(data$x)));
                       } else {
                           warning(">> fit$LateExpT0GammaInt2 does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (exists(x = "Auto", where = fit)) {
                           return(get_drv2(fit$Auto_model));
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
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_drv2
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin",
    value = compiler::cmpfun(
        f = function(tg.model, time = NULL) {
            switch(tg.model,
                   "None" = {
                       return(rep(NA_real_, length(data$x)));
                   },
                   "Gamma" = {
                       if (exists(x = "Gamma", where = fit)) {
                           A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
                           theta <- fit$Gamma$cff[["theta"]];
                           return(A * dgamma(x = data$x, shape = k, scale = theta));
                       } else {
                           warning(">> fit$Gamma does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "T0Gamma" = {
                       if (exists(x = "T0Gamma", where = fit)) {
                           A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                           theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
                           return(A * dgamma(x = data$x - t0, shape = k, scale = theta));
                       } else {
                           warning(">> fit$T0Gamma does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "GammaInt" = {
                       if (exists(x = "GammaInt", where = fit)) {
                           A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                           theta <- fit$GammaInt$cff[["theta"]];
                           return(A * dgamma(x = data$x, shape = k, scale = theta));
                       } else {
                           warning(">> fit$GammaInt does not exist!");
                   return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "T0GammaInt" = {
                       if (exists(x = "T0GammaInt", where = fit)) {
                           A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                           theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                           return(A * dgamma(x = data$x - t0, shape = k, scale = theta));
                       } else {
                           warning(">> fit$T0GammaInt does not exist!");
                           return(rep(NA_real_, length(data$x)));
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
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (exists(x = "T0GammaInt2_test", where = fit)) {
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           if (!is.null(time)) {
                               return(A1 * dgamma(x = time - t0, shape = k1, scale = theta1) +
                                          A2 * dgamma(x = time - t0, shape = k2, scale = theta2));
                           } else {
                               return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta1) +
                                          A2 * dgamma(x = data$x - t0, shape = k2, scale = theta2));
                           }
                       } else {
                           warning(">> fit$T0GammaInt2_test does not exist!");
                           return(rep(NA_real_, length(data$x)));
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
                           return(rep(NA_real_, length(data$x)));
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
                           return(rep(NA_real_, length(data$x)));
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
                           ## return(p1 * exp(-(A * pgamma(q = data$x - t0, shape = k, scale = theta) +
                           ##                       (A * k.a2m / gamma(k)) * (
                           ##                           gamma(k) *
                           ##                               pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                           ##                                   gamma(k + 1) * theta *
                           ##                                       pgamma(q = data$x - t0, shape = k + 1, scale = theta))
                           ##                   )) * A * dgamma(x = data$x - t0, shape = k, scale = theta)
                           ##        );
                           return(p1 * (
                               A1 * dgamma(x = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * dgamma(x = data$x - t0, shape = k2,
                                           scale = theta)
                           ));
                       } else {
                           warning(">> fit$LateExpT0GammaInt2 does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (exists(x = "Auto", where = fit)) {
                           return(get_thrombin(fit$Auto_model));
                       } else {
                           warning(">> fit$Auto does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   "Pade" = {
                       if (exists(x = "Pade", where = fit)) {
#### checking thrombogram vs Henrik's thrombogram
                           bgr <- fit$Pade$cff[["bgr"]];
                           a <- fit$Pade$cff[["a"]]; b <- fit$Pade$cff[["b"]];
                           q1 <- fit$Pade$cff[["q1"]]; p3 <- fit$Pade$cff[["p3"]];
                           q2 <- fit$Pade$cff[["q2"]]; p4 <- fit$Pade$cff[["p4"]];
                           q3 <- fit$Pade$cff[["q3"]]; p5 <- fit$Pade$cff[["p5"]];
                           q4 <- fit$Pade$cff[["q4"]];
                           p0 <- -log(b); p1 <- -(log(b) * q1 + (a / b));
                           p2 <- -q2 * log(b) - (q1 * a / b) + 0.5 * (a / b) ^ 2;
                           ## calculate p2 from Pade fit
                           time <- data$x;
                           p2 <- (
                               Pade54Drv2(x = time, p0 = p0, p1 = p1, p2 = p2,
                                          q1 = q1, p3 = p3, q2 = q2,
                                          p4 = p4, q3 = q3, p5 = p5, q4 = q4) -
                               (Pade54Drv1(x = time, p0 = p0, p1 = p1, p2 = p2,
                                           q1 = q1, p3 = p3, q2 = q2,
                                           p4 = p4, q3 = q3, p5 = p5, q4 = q4)) ^ 2
                           ) *
                               exp(-Pade54(x = time, p0 = p0, p1 = p1, p2 = p2,
                                           q1 = q1, p3 = p3, q2 = q2,
                                           p4 = p4, q3 = q3, p5 = p5, q4 = q4)); #print("p2 = ");  # print(p2);
                           ## define function T0(k.a2m)
                           T0 <- function(k.a2m = 0, time, p2) {
                               return(sum(exp(k.a2m * time) * p2) -
                                      0.5 * (exp(k.a2m * time[length(time)]) * p2[length(time)] +
                                             exp(k.a2m * time[1]) * p2[1]))
                           }
                           ## find the null of the T0
                           k.a2m <- a / b;
                           ## print(paste0(">> k.a2m from Pade = ", k.a2m));
                           res <- uniroot(f = T0, interval = c(0.5 * k.a2m, 1.5 * k.a2m),
                                          time = time, p2 = p2,
                                          ## f.lower = tvelpeak_est_f(t = t1, A1 = A1, A2 = A2,
                                          ##                          k1 = k1, k2 = k2,
                                          ##                          theta = theta, t0 = t0),
                                          ## f.upper = tvelpeak_est_f(t = t2, A1 = A1, A2 = A2,
                                          ##                          k1 = k1, k2 = k2,
                                          ##                          theta = theta, t0 = t0),
                                          trace = 1, extendInt = "yes");
                           ## print(paste0("k.a2m from uniroot: ", res$root));
                           ## calculate thrombogram using k.a2m from uniroot
                           GetIntgrl <- function(t, k.a2m, time, p2, t.max) {
                               t.prime <- time[time >= t]; p2.prime <- p2[time >= t];
                               N.prime <- length(t.prime);
                               if (N.prime >= 3) {
                                   return(sum(exp(k.a2m * t.prime) * p2.prime) -
                                          0.5 * (exp(k.a2m * t.prime[1]) * p2.prime[1] +
                                                 exp(k.a2m * t.prime[N.prime]) * p2.prime[N.prime])
                                          );
                               } else if (N.prime == 2) {
                                   return(0.5 * (exp(k.a2m * t.prime[1]) * p2.prime[1] +
                                                 exp(k.a2m * t.prime[2]) * p2.prime[2]));
                               } else {
                                   return(0.0);
                               }
                           }
                           HenrThromb <- function(t, k.a2m, time, p2) {
                               dt <- time[2] - time[1];
                               return(-exp(-k.a2m * t) * dt *
                                      GetIntgrl(t, k.a2m, time, p2, max(time)));
                           }
                           henr.thromb <- rep(0.0, length = length(data$x));
                           for (i in 1:length(data$x)) {
                               henr.thromb[i] <- HenrThromb(data$x[i], res$root, data$x, p2);
                           }
                           ## print("henr.thromb = "); # print(henr.thromb);
                           ## lines(data$x, log(henr.thromb), col = "green", lwd = 4, lty = 3);
                           ## print("henr.thromb / thromb = "); #print(henr.thromb / get_thrombin(tg.model));
                           ##                            return(henr.thromb);
                           return(henr.thromb);
                           ## return(rep(NA_real_, length(data$x)));
                       } else {
                           warning(">> fit$Pade does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   {  ## Default
                       warning(paste0(">> Call to unknown tg.model ", tg.model));
                       return(rep(NA_real_, length(data$x)));
                   }
                   );  ## End of switch(tg.model)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_thrombin_contribution",
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
                                   return(A1 * dgamma(x = time - t0, shape = k1, scale = theta1));
                               } else {
                                   return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta1));
                               }
                           } else if (number == 2) {
                               if (!is.null(time)) {
                                   return(A2 * dgamma(x = time - t0, shape = k2, scale = theta2));
                               } else {
                                   return(A2 * dgamma(x = data$x - t0, shape = k2, scale = theta2));
                               }
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
                               if (!is.null(time)) {
                                   return(p1 * A1 *
                                          dgamma(x = time - t0,
                                                 shape = k1, scale = theta));
                               } else {
                                   return(p1 * A1 *
                                          dgamma(x = data$x - t0,
                                                 shape = k1, scale = theta));
                               }
                           } else if (number == 2) {
                               if (!is.null(time)) {
                                   return(p1 * A2 *
                                          dgamma(x = time - t0, shape = k2,
                                                 scale = theta));
                               } else {
                                   return(p1 * A2 *
                                          dgamma(x = data$x - t0, shape = k2,
                                                 scale = theta));
                               }
                           }
                       }
                   }
                   );
            ## if (tg.model == "T0GammaInt2") {
            ##     if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
            ##         A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
            ##         k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
            ##         theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
            ##         if (number == 1) {
            ##             if (!is.null(time)) {
            ##                 return(A1 * dgamma(x = time - t0, shape = k1, scale = theta));
            ##             } else {
            ##                 return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta));
            ##             }
            ##         } else if (number == 2) {
            ##             if (!is.null(time)) {
            ##                 return(A2 * dgamma(x = time - t0, shape = k2, scale = theta));
            ##             } else {
            ##                 return(A2 * dgamma(x = data$x - t0, shape = k2, scale = theta));
            ##             }
            ##         }
            ##     }
            ## }
            return(rep(0, length(data$x)));
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_thrombin_contribution
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_drv1",
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
                           theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
                           return(A * dgamma(x = data$x, shape = k, scale = theta) +
                                      k.a2m * A * pgamma(q = data$x, shape = k, scale = theta));
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
                           return(A * dgamma(x = data$x - t0, shape = k, scale = theta) +
                                      k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta));
                       } else {
                           warning(">> fit$T0GammaInt does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "T0GammaInt2" = {
                       if (exists(x = "T0GammaInt2", where = fit)) {
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]]; k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt2$cff[["t0"]];
                           return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta) +
                                      A2 * dgamma(x = data$x - t0, shape = k2, scale = theta) + 
                                          k.a2m * A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
                                              k.a2m * A2 * pgamma(q = data$x - t0, shape = k2, scale = theta)
                                  );
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
                           return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta1) +
                                      A2 * dgamma(x = data$x - t0, shape = k2, scale = theta2) + 
                                          k.a2m * A1 * pgamma(q = data$x - t0, shape = k1, scale = theta1) +
                                              k.a2m * A2 * pgamma(q = data$x - t0, shape = k2, scale = theta2)
                                  );
                       } else {
                           warning(">> fit$T0GammaInt2_test does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "LateExpGammaInt" = {
                       if (exists(x = "LateExpGammaInt", where = fit)) {
                           p1 <- fit$LateExpGammaInt$cff[["p1"]];
                           A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                           theta <- fit$LateExpGammaInt$cff[["theta"]]; k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                           return(p1 * exp(-(A * pgamma(q = data$x, shape = k, scale = theta) +
                                                 (A * k.a2m / gamma(k)) * (
                                                     gamma(k) *
                                                         pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
                                                             gamma(k + 1) * theta *
                                                                 pgamma(q = data$x, shape = k + 1, scale = theta))
                                             )) * (A * dgamma(x = data$x, shape = k, scale = theta) +
                                                       k.a2m * A * pgamma(q = data$x, shape = k, scale = theta))
                                  );
                       } else {
                           warning(">> fit$LateExpGammaInt does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt" = {
                       if (exists(x = "LateExpT0GammaInt", where = fit)) {
                           p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           theta <- fit$LateExpT0GammaInt$cff[["theta"]]; k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                           t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                           return(p1 * exp(-(A * pgamma(q = data$x - t0, shape = k, scale = theta) +
                                                 (A * k.a2m / gamma(k)) * (
                                                     gamma(k) *
                                                         pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                                                             gamma(k + 1) * theta *
                                                                 pgamma(q = data$x - t0, shape = k + 1, scale = theta))
                                             )) * (A * dgamma(x = data$x - t0, shape = k, scale = theta) +
                                                       k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta))
                                  );
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
                           return(p1 * exp(-(
                               A1 * pgamma(q = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * pgamma(q = data$x - t0, shape = k2,
                                           scale = theta) +
                               (A1 * k.a2m / gamma(k1)) * (
                                   gamma(k1) *
                                   pgamma(q = data$x - t0, shape = k1,
                                          scale = theta) * (data$x - t0) -
                                   gamma(k1 + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k1 + 1,
                                          scale = theta)) +
                               (A2 * k.a2m / gamma(k2)) * (
                                   gamma(k2) *
                                   pgamma(q = data$x - t0, shape = k2,
                                          scale = theta) * (data$x - t0) -
                                   gamma(k2 + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k2 + 1,
                                          scale = theta))
                           )) * (
                               A1 * dgamma(x = data$x - t0, shape = k1,
                                           scale = theta) +
                               A2 * dgamma(x = data$x - t0, shape = k2,
                                           scale = theta) +
                               k.a2m * A1 * pgamma(q = data$x - t0, shape = k1,
                                                   scale = theta) +
                               k.a2m * A2 * pgamma(q = data$x - t0, shape = k2,
                                                   scale = theta)
                           )
                           );
                       } else {
                           warning(">> fit$LateExpT0GammaInt2 does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (exists(x = "Auto", where = fit)) {
                           return(get_drv1(fit$Auto_model));
                       } else {
                           warning(">> fit$Auto does not exist!");
                           return(rep(0, length(data$x)));
                       }
                   },
                   "Pade" = {
                       if (exists(x = "Pade", where = fit)) {
                           bgr <- fit$Pade$cff[["bgr"]];
                           a <- fit$Pade$cff[["a"]]; b <- fit$Pade$cff[["b"]];
                           q1 <- fit$Pade$cff[["q1"]]; p3 <- fit$Pade$cff[["p3"]];
                           q2 <- fit$Pade$cff[["q2"]]; p4 <- fit$Pade$cff[["p4"]];
                           q3 <- fit$Pade$cff[["q3"]]; p5 <- fit$Pade$cff[["p5"]];
                           q4 <- fit$Pade$cff[["q4"]];
                           p0 <- -log(b); p1 <- -(log(b) * q1 + (a / b));
                           p2 <- -q2 * log(b) - (q1 * a / b) + 0.5 * (a / b) ^ 2;                           
                           return(a + Pade54Drv1(x = data$x, p0 = p0, p1 = p1,
                                                 p2 = p2, q1 = q1, p3 = p3,
                                                 q2 = q2, p4 = p4, q3 = q3,
                                                 p5 = p5, q4 = q4) *
                                  exp(-Pade54(x = data$x, p0 = p0, p1 = p1,
                                              p2 = p2, q1 = q1, p3 = p3,
                                              q2 = q2, p4 = p4, q3 = q3,
                                              p5 = p5, q4 = q4)));
                       } else {
                           warning(">> fit$Pade does not exist!");
                           return(rep(NA_real_, length(data$x)));
                       }
                   },
                   {  ## Default
                       warning(paste0(">> In get_drv1: call to unknown tg.model ",
                                      tg.model));
                       return(rep(NA_real_, length(data$x)));
                   }
                   );  ## End of switch(tg.model)
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_drv1
################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.get_drv1 <- function(tg.model) {
##     switch(tg.model,
##            "None" = {
##                return(rep(0, length(data$x)));
##            },
##            "Gamma" = {
##                if (exists(x = "Gamma", where = fit)) {
##                    A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
##                    theta <- fit$Gamma$cff[["theta"]];
##                    return(A * dgamma(x = data$x, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$Gamma does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "T0Gamma" = {
##                if (exists(x = "T0Gamma", where = fit)) {
##                    A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
##                    theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
##                    return(A * dgamma(x = data$x - t0, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$T0Gamma does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "GammaInt" = {
##                if (exists(x = "GammaInt", where = fit)) {
##                    A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
##                    theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
##                    return(A * dgamma(x = data$x, shape = k, scale = theta) +
##                               k.a2m * A * pgamma(q = data$x, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$GammaInt does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "T0GammaInt" = {
##                if (exists(x = "T0GammaInt", where = fit)) {
##                    A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
##                    theta <- fit$T0GammaInt$cff[["theta"]]; k.a2m <- fit$T0GammaInt$cff[["k.a2m"]];
##                    t0 <- fit$T0GammaInt$cff[["t0"]];
##                    return(A * dgamma(x = data$x - t0, shape = k, scale = theta) +
##                               k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta));
##                } else {
##                    warning(">> fit$T0GammaInt does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "T0GammaInt2" = {
##                if (exists(x = "T0GammaInt2", where = fit)) {
##                    A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
##                    k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
##                    theta <- fit$T0GammaInt2$cff[["theta"]]; k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]];
##                    t0 <- fit$T0GammaInt2$cff[["t0"]];
##                    return(A1 * dgamma(x = data$x - t0, shape = k1, scale = theta) +
##                               A2 * dgamma(x = data$x - t0, shape = k2, scale = theta) + 
##                                   k.a2m * A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
##                                       k.a2m * A2 * pgamma(q = data$x - t0, shape = k2, scale = theta)
##                           );
##                } else {
##                    warning(">> fit$T0GammaInt2 does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "LateExpGammaInt" = {
##                if (exists(x = "LateExpGammaInt", where = fit)) {
##                    p1 <- fit$LateExpGammaInt$cff[["p1"]];
##                    A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
##                    theta <- fit$LateExpGammaInt$cff[["theta"]]; k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
##                    return(p1 * exp(-(A * pgamma(q = data$x, shape = k, scale = theta) +
##                                          (A * k.a2m / gamma(k)) * (
##                                              gamma(k) *
##                                                  pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
##                                                      gamma(k + 1) * theta *
##                                                          pgamma(q = data$x, shape = k + 1, scale = theta))
##                                      )) * (A * dgamma(x = data$x, shape = k, scale = theta) +
##                                                k.a2m * A * pgamma(q = data$x, shape = k, scale = theta))
##                           );
##                } else {
##                    warning(">> fit$LateExpGammaInt does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "LateExpT0GammaInt" = {
##                if (exists(x = "LateExpT0GammaInt", where = fit)) {
##                    p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
##                    A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
##                    theta <- fit$LateExpT0GammaInt$cff[["theta"]]; k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
##                    t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
##                    return(p1 * exp(-(A * pgamma(q = data$x - t0, shape = k, scale = theta) +
##                                          (A * k.a2m / gamma(k)) * (
##                                              gamma(k) *
##                                                  pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
##                                                      gamma(k + 1) * theta *
##                                                          pgamma(q = data$x - t0, shape = k + 1, scale = theta))
##                                      )) * (A * dgamma(x = data$x - t0, shape = k, scale = theta) +
##                                                k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta))
##                           );
##                } else {
##                    warning(">> fit$LateExpT0GammaInt does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            "Auto" = {
##                if (exists(x = "Auto", where = fit)) {
##                    return(get_drv1(fit$Auto_model));
##                } else {
##                    warning(">> fit$Auto does not exist!");
##                    return(rep(0, length(data$x)));
##                }
##            },
##            {  ## Default
##                warning(paste0(">> Call to unknown tg.model ", tg.model));
##                return(rep(0, length(data$x)));
##            }
##            );  ## End of switch(tg.model)
## }  ## End of TG.get_drv1
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

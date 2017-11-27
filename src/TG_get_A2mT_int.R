################################################################################
TG$set(
    which = "public", name = "get_A2mT_int",
    value = compiler::cmpfun(
        f =  function(tg.model) {
            switch(tg.model,
                   "None" = {
                       return(rep(NA, length(data$x)));
                   },
                   "Gamma" = {
                       if (is.null(fit$Gamma)) {  ## !exists(x = "Gamma", where = fit)
                           print(">> fit$Gamma does not exist!");
                       }
                       return(rep(NA, length(data$x)));
                   },
                   "T0Gamma" = {
                       if (is.null(fit$T0Gamma)) {  ## !exists(x = "T0Gamma", where = fit)
                           print(">> fit$T0Gamma does not exist!");
                       }
                       return(rep(NA, length(data$x)));
                   },
                   "GammaInt" = {
                       if (!is.null(fit$GammaInt)) {  ## exists(x = "GammaInt", where = fit)
                           A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                           theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
                           return((A * k.a2m / gamma(k)) * (
                               gamma(k) *
                                   pgamma(q = data$x, shape = k, scale = theta) * data$x -
                                       gamma(k + 1) * theta *
                                           pgamma(q = data$x, shape = k + 1, scale = theta)));
                       } else {
                           print(">> fit$GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt" = {
                       if (!is.null(fit$T0GammaInt)) {  ## exists(x = "T0GammaInt", where = fit)
                           ## print(fit$T0GammaInt);
                           A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                           theta <- fit$T0GammaInt$cff[["theta"]];
                           k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                           return((A * k.a2m / gamma(k)) * (
                               gamma(k) *
                                   pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                                       gamma(k + 1) * theta *
                                           pgamma(q = data$x - t0, shape = k + 1, scale = theta)));
                       } else {
                           print(">> fit$T0GammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt2" = {
                       if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
                           A1 <- fit$T0GammaInt2$cff[["A1"]]; A2 <- fit$T0GammaInt2$cff[["A2"]];
                           k1 <- fit$T0GammaInt2$cff[["k1"]]; k2 <- fit$T0GammaInt2$cff[["k2"]];
                           theta <- fit$T0GammaInt2$cff[["theta"]]; k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt2$cff[["t0"]];
                           return(
                               (A1 * k.a2m / gamma(k1)) * (
                                   gamma(k1) *
                                       pgamma(q = data$x - t0, shape = k1, scale = theta) * (data$x - t0) -
                                           gamma(k1 + 1) * theta *
                                               pgamma(q = data$x - t0, shape = k1 + 1, scale = theta)) +
                                   (A2 * k.a2m / gamma(k2)) * (
                                       gamma(k2) *
                                           pgamma(q = data$x - t0, shape = k2, scale = theta) * (data$x - t0) -
                                               gamma(k2 + 1) * theta *
                                                   pgamma(q = data$x - t0, shape = k2 + 1, scale = theta))
                               );
                       } else {
                           print(">> fit$T0GammaInt2 does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "T0GammaInt2_test" = {
                       if (!is.null(fit$T0GammaInt2_test)) {  ## exists(x = "T0GammaInt2_test", where = fit)
                           A1 <- fit$T0GammaInt2_test$cff[["A1"]]; A2 <- fit$T0GammaInt2_test$cff[["A2"]];
                           k1 <- fit$T0GammaInt2_test$cff[["k1"]]; k2 <- fit$T0GammaInt2_test$cff[["k2"]];
                           theta1 <- fit$T0GammaInt2_test$cff[["theta1"]]; theta2 <- fit$T0GammaInt2_test$cff[["theta2"]];
                           k.a2m <- fit$T0GammaInt2_test$cff[["k.a2m"]];
                           t0 <- fit$T0GammaInt2_test$cff[["t0"]];
                           return(
                               (A1 * k.a2m / gamma(k1)) * (
                                   gamma(k1) *
                                       pgamma(q = data$x - t0, shape = k1, scale = theta1) * (data$x - t0) -
                                           gamma(k1 + 1) * theta1 *
                                               pgamma(q = data$x - t0, shape = k1 + 1, scale = theta1)) +
                                   (A2 * k.a2m / gamma(k2)) * (
                                       gamma(k2) *
                                           pgamma(q = data$x - t0, shape = k2, scale = theta2) * (data$x - t0) -
                                               gamma(k2 + 1) * theta2 *
                                                   pgamma(q = data$x - t0, shape = k2 + 1, scale = theta2))
                               );
                       } else {
                           print(">> fit$T0GammaInt2_test does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpGammaInt" = {
                       if (!is.null(fit$LateExpGammaInt)) {  ## exists(x = "LateExpGammaInt", where = fit)
                           A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                           theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                           k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                           ## return(p1 * (1 - exp(-(A * k.a2m / gamma(k)) * (
                           ##     gamma(k) *
                           ##         pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
                           ##             gamma(k + 1) * theta *
                           ##                 pgamma(q = data$x, shape = k + 1, scale = theta)))));
                           return(p1 * (A * k.a2m / gamma(k)) * (
                               gamma(k) *
                                   pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
                                       gamma(k + 1) * theta *
                                           pgamma(q = data$x, shape = k + 1, scale = theta)));
                       } else {
                           print(">> fit$LateExpGammaInt does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "LateExpT0GammaInt" = {
                       if (!is.null(fit$LateExpT0GammaInt)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                           A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                           theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                           k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]]; t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                           ## return(p1 * (1 - exp(-(A * k.a2m / gamma(k)) * (
                           ##     gamma(k) *
                           ##         pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                           ##             gamma(k + 1) * theta *
                           ##                 pgamma(q = data$x - t0, shape = k + 1, scale = theta)))));
                           return(p1 * (A * k.a2m / gamma(k)) * (
                               gamma(k) *
                                   pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                                       gamma(k + 1) * theta *
                                           pgamma(q = data$x - t0, shape = k + 1, scale = theta)));
                       } else {
                           print(">> fit$LateExpT0GammaInt does not exist!");
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
                           k.a2m <- fit$LateExpT0GammaInt2$cff[["k.a2m"]];
                           t0 <- fit$LateExpT0GammaInt2$cff[["t0"]];
                           ## return(p1 * (1 - exp(-(A * k.a2m / gamma(k)) * (
                           ##     gamma(k) *
                           ##         pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                           ##             gamma(k + 1) * theta *
                           ##                 pgamma(q = data$x - t0, shape = k + 1, scale = theta)))));
                           return(p1 * (
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
                           ));
                       } else {
                           print(">> fit$LateExpT0GammaInt2 does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   "Auto" = {
                       if (!is.null(fit$Auto)) {  ## exists(x = "Auto", where = fit)
                           return(get_A2mT_int(fit$Auto_model));
                       } else {
                           print(">> fit$Auto does not exist!");
                           return(rep(NA, length(data$x)));
                       }
                   },
                   { print(paste0(">> Call to unknown tg.model ", tg.model));
                       return(rep(NA, length(data$x))); }
                   );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_A2mT_int
################################################################################

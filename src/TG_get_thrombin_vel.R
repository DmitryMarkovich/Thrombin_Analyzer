################################################################################
TG.get_thrombin_vel <- function(tg.model) {
    switch(tg.model,
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
                   theta <- fit$GammaInt$cff[["theta"]];
                   v <- A * exp(-data$x / theta) * data$x ^ (k - 1) *
                       ((-1 / theta) + ((k - 1) / data$x)) /
                           (gamma(k) * theta ^ k);
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
                   theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                   v <- A * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k - 1) *
                       ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           (gamma(k) * theta ^ k);
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
                   theta <- fit$T0GammaInt2$cff[["theta"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                   v <- A1 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k1 - 1) *
                       ((-1 / theta) + ((k1 - 1) / (data$x - t0))) /
                           (gamma(k1) * theta ^ k1) +
                               A2 * exp(-(data$x - t0) / theta) * (data$x - t0) ^ (k2 - 1) *
                                   ((-1 / theta) + ((k2 - 1) / (data$x - t0))) /
                                       (gamma(k2) * theta ^ k2);
                   v[data$x <= t0] <- 0;
                   return(v);
               } else {
                   warning(">> fit$T0GammaInt2 does not exist!");
                   return(rep(0, length(data$x)));
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
                   return(rep(0, length(data$x)));
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
                   return(rep(0, length(data$x)));
               }
           },
           "Auto" = {
               if (exists(x = "Auto", where = fit)) {
                   return(get_thrombin_vel(fit$Auto_model));
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
}  ## End of TG.get_thrombin_vel
################################################################################

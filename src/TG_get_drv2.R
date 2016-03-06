################################################################################
TG.get_drv2 <- function(tg.model) {
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
                       ((-1 / theta) + ((k - 1) / (data$x - t0))) /
                           (gamma(k) * theta ^ k) +
                               k.a2m * A * dgamma(x = data$x - t0, shape = k,
                                                  scale = theta);
                   v[data$x <= t0] <- 0;
                   return(v);
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
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
}  ## End of TG.get_drv2
################################################################################

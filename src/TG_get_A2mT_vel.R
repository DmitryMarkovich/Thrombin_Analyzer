################################################################################
TG.get_A2mT_vel <- function(tg.model) {
    switch(tg.model,
           "Gamma" = {
               if (!exists(x = "Gamma", where = fit))
                   warning(">> fit$Gamma does not exist!");
               return(rep(0, length(data$x)));
           },
           "T0Gamma" = {
               if (!exists(x = "T0Gamma", where = fit))
                   warning(">> fit$T0Gamma does not exist!");
               return(rep(0, length(data$x)));
           },
           "GammaInt" = {
               if (exists(x = "GammaInt", where = fit)) {
                   A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                   theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
                   v <- k.a2m * A * dgamma(x = data$x, shape = k, scale = theta);
                   ## v[data$x == 0] <- 0;
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
                   v <- k.a2m * A * dgamma(x = data$x - t0, shape = k, scale = theta);
                   ## v[data$x == 0] <- 0;
                   return(v);
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           { warning(">> Unknown model called!"); return(rep(0, length(data$x)));}
           );  ## End of switch(tg.model)
}  ## End of TG.get_A2mT_vel
################################################################################

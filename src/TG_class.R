################################################################################
TG.get_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = get_Gamma(), "T0Gamma" = get_T0Gamma(),
           "GammaInt" = get_GammaInt(), "T0GammaInt" = get_T0GammaInt(),
           "LateExpGammaInt" = get_LateExpGammaInt(),
           "LateExpT0GammaInt" = get_LateExpT0GammaInt(),
           { print(paste0(">> Call to unknown get_model ", tg.model))}
           );
}  ## End of TG.get_model
################################################################################

################################################################################
TG.get_thrombin_int <- function(tg.model) {
    switch(tg.model,
           "Gamma" = {
               if (exists(x = "Gamma", where = fit)) {
                   A <- fit$Gamma$cff[["A"]]; k <- fit$Gamma$cff[["k"]];
                   theta <- fit$Gamma$cff[["theta"]];
                   return(A * pgamma(q = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$Gamma does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0Gamma" = {
               if (exists(x = "T0Gamma", where = fit)) {
                   A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                   theta <- fit$T0Gamma$cff[["theta"]]; t0 <- fit$T0Gamma$cff[["t0"]];
                   return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$T0Gamma does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "GammaInt" = {
               if (exists(x = "GammaInt", where = fit)) {
                   A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                   theta <- fit$GammaInt$cff[["theta"]];
                   return(A * pgamma(q = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0GammaInt" = {
               if (exists(x = "T0GammaInt", where = fit)) {
                   A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                   theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                   return(A * pgamma(q = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpGammaInt" = {
               if (exists(x = "LateExpGammaInt", where = fit)) {
                   A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                   theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                   return(p1 * (1 - exp(-A * pgamma(q = data$x, shape = k, scale = theta))));
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
                   return(p1 * (1 - exp(-A * pgamma(q = data$x - t0, shape = k, scale = theta))));
               } else {
                   warning(">> fit$LateExpT0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           { print(paste0(">> Call to unknown tg.model", tg.model))}
           );
}  ## End of TG.get_thrombin_int
################################################################################

################################################################################
TG.get_A2mT_int <- function(tg.model) {
    switch(tg.model,
           "Gamma" = {
               if (!exists(x = "Gamma", where = fit)) {
                   warning(">> fit$Gamma does not exist!");
               }
               return(rep(0, length(data$x)));
           },
           "T0Gamma" = {
               if (!exists(x = "T0Gamma", where = fit)) {
                   warning(">> fit$T0Gamma does not exist!");
               }
               return(rep(0, length(data$x)));
           },
           "GammaInt" = {
               if (exists(x = "GammaInt", where = fit)) {
                   A <- fit$GammaInt$cff[["A"]]; k <- fit$GammaInt$cff[["k"]];
                   theta <- fit$GammaInt$cff[["theta"]]; k.a2m <- fit$GammaInt$cff[["k.a2m"]];
                   return((A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x, shape = k, scale = theta) * data$x -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x, shape = k + 1, scale = theta)));
               } else {
                   warning(">> fit$GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0GammaInt" = {
               if (exists(x = "T0GammaInt", where = fit)) {
                   A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                   theta <- fit$T0GammaInt$cff[["theta"]];
                   k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                   return((A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k + 1, scale = theta)));
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpGammaInt" = {
               if (exists(x = "LateExpGammaInt", where = fit)) {
                   A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                   theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                   k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                   return(p1 * (1 - exp(-(A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x, shape = k, scale = theta) * (data$x) -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x, shape = k + 1, scale = theta)))));
               } else {
                   warning(">> fit$LateExpGammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpT0GammaInt" = {
               if (exists(x = "LateExpT0GammaInt", where = fit)) {
                   A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                   theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                   k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]]; t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                   return(p1 * (1 - exp(-(A * k.a2m / gamma(k)) * (
                       gamma(k) *
                           pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                               gamma(k + 1) * theta *
                                   pgamma(q = data$x - t0, shape = k + 1, scale = theta)))));
               } else {
                   warning(">> fit$LateExpT0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           { print(paste0(">> Call to unknown tg.model ", tg.model))}
           );
}  ## End of TG.get_A2mT_int
################################################################################

################################################################################
TG.get_thrombin <- function(tg.model) {
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
           "LateExpGammaInt" = {
               if (exists(x = "LateExpGammaInt", where = fit)) {
                   A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                   theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                   return(p1 * (1 - exp(-A * dgamma(x = data$x, shape = k, scale = theta))));
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
                   return(p1 * (1 - exp(-A * dgamma(x = data$x - t0, shape = k, scale = theta))));
               } else {
                   warning(">> fit$LateExpT0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           { print(paste0(">> Call to unknown tg.model ", tg.model))}
           );  ## End of switch(tg.model)
}  ## End of TG.get_thrombin
################################################################################

################################################################################
TG.get_A2mT <- function(tg.model) {
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
                   return(k.a2m * A * pgamma(q = data$x, shape = k, scale = theta));
               } else {
                   warning(">> fit$GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "T0GammaInt" = {
               if (exists(x = "T0GammaInt", where = fit)) {
                   A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
                   theta <- fit$T0GammaInt$cff[["theta"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                   k.a2m <- fit$T0GammaInt$cff[["k.a2m"]];
                   return(k.a2m * A * pgamma(q = data$x - t0, shape = k, scale = theta));
               } else {
                   warning(">> fit$T0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpGammaInt" = {
               if (exists(x = "LateExpGammaInt", where = fit)) {
                   A <- fit$LateExpGammaInt$cff[["A"]]; k <- fit$LateExpGammaInt$cff[["k"]];
                   theta <- fit$LateExpGammaInt$cff[["theta"]]; p1 <- fit$LateExpGammaInt$cff[["p1"]];
                   k.a2m <- fit$LateExpGammaInt$cff[["k.a2m"]];
                   return(k.a2m * p1 * (1 - exp(-A * dgamma(x = data$x, shape = k, scale = theta))));
               } else {
                   warning(">> fit$LateExpGammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           "LateExpT0GammaInt" = {
               if (exists(x = "LateExpT0GammaInt", where = fit)) {
                   A <- fit$LateExpT0GammaInt$cff[["A"]]; k <- fit$LateExpT0GammaInt$cff[["k"]];
                   theta <- fit$LateExpT0GammaInt$cff[["theta"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                   k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                   t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                   return(k.a2m * p1 * (1 - exp(-A * dgamma(x = data$x - t0, shape = k, scale = theta))));
               } else {
                   warning(">> fit$LateExpT0GammaInt does not exist!");
                   return(rep(0, length(data$x)));
               }
           },
           { print(paste0(">> Call to unknown tg.model ", tg.model))}
           );  ## End of switch(tg.model)
}  ## End of TG.get_A2mT
################################################################################

################################################################################
source("src/TG_plotting_methods.R");
################################################################################
TG.fit_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = fit_Gamma(silent = FALSE), "T0Gamma" = fit_T0Gamma(silent = FALSE),
           "GammaInt" = fit_GammaInt(silent = FALSE), "T0GammaInt" = fit_T0GammaInt(silent = FALSE),
           "LateExpGammaInt" = fit_LateExpGammaInt(silent = FALSE),
           "LateExpT0GammaInt" = fit_LateExpT0GammaInt(silent = FALSE),
           { print(paste0(">> Call to unknown model", tg.model))}
           );
}  ## End of TG.fit_model
################################################################################
source("src/TG_fit_Gamma.R"); source("src/TG_fit_T0Gamma.R");
source("src/TG_fit_GammaInt.R"); source("src/TG_fit_T0GammaInt.R");
source("src/TG_fit_LateExpGammaInt.R"); source("src/TG_fit_LateExpT0GammaInt.R");
################################################################################
TG.parms_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = parms_Gamma(), "T0Gamma" = parms_T0Gamma(),
           "GammaInt" = parms_GammaInt(), "T0GammaInt" = parms_T0GammaInt(),
           "LateExpGammaInt" = parms_LateExpGammaInt(), "LateExpT0GammaInt" = parms_LateExpT0GammaInt(),
           { print(paste0(">> Call to unknown model ", tg.model))}
           );
}  ## End of TG.parms_model
################################################################################

################################################################################
TG <- setRefClass(
    Class = "TG", contains = "Base",
    fields = list(
        data = "data.frame", num.smry = "list", fit = "list",
        parms = "data.frame"
    ),
    methods = list(
        clear = TG.clear, plot = TG.plot,
        plot_drv1 = TG.plot_drv1, plot_drv2 = TG.plot_drv2,
        ## Gamma
        fit_Gamma = TG.fit_Gamma, get_Gamma = TG.get_Gamma,
        parms_Gamma = TG.parms_Gamma,
        ## T0Gamma
        fit_T0Gamma = TG.fit_T0Gamma, get_T0Gamma = TG.get_T0Gamma,
        parms_T0Gamma = TG.parms_T0Gamma,
        ## GammaInt
        fit_GammaInt = TG.fit_GammaInt, get_GammaInt = TG.get_GammaInt,
        parms_GammaInt = TG.parms_GammaInt,
        ## T0GammaInt
        fit_T0GammaInt = TG.fit_T0GammaInt, get_T0GammaInt = TG.get_T0GammaInt,
        parms_T0GammaInt = TG.parms_T0GammaInt,
        ## LateExpGammaInt
        fit_LateExpGammaInt = TG.fit_LateExpGammaInt, get_LateExpGammaInt = TG.get_LateExpGammaInt,
        parms_LateExpGammaInt = TG.parms_LateExpGammaInt,
        ## LateExpT0GammaInt
        fit_LateExpT0GammaInt = TG.fit_LateExpT0GammaInt, get_LateExpT0GammaInt = TG.get_LateExpT0GammaInt,
        parms_LateExpT0GammaInt = TG.parms_LateExpT0GammaInt,
        ## model
        fit_model = TG.fit_model, get_model = TG.get_model, parms_model = TG.parms_model,
        get_thrombin_int = TG.get_thrombin_int, get_A2mT_int = TG.get_A2mT_int,
        get_thrombin = TG.get_thrombin, get_A2mT = TG.get_A2mT,
        plot_fit = TG.plot_fit, plot_thrombogram = TG.plot_thrombogram
    )
);  ## End of TG setRefClass
################################################################################

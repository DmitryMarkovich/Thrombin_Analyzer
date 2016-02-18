################################################################################
TG.get_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = get_Gamma(), "T0Gamma" = get_T0Gamma(),
           "GammaInt" = get_GammaInt(), "T0GammaInt" = get_T0GammaInt(),
           { print(paste0(">> Call to unknown get_model ", tg.model))}
           );
}  ## End of TG.get_model
################################################################################
source("src/TG_plotting_methods.R");
################################################################################
TG.fit_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = fit_Gamma(silent = FALSE),
           "T0Gamma" = fit_T0Gamma(silent = FALSE),
           "GammaInt" = fit_GammaInt(silent = FALSE),
           "T0GammaInt" = fit_T0GammaInt(silent = FALSE),
           ## "EarlyMM" = fit_EarlyMM(),
           ## "LateExp" = fit_LateExp(),
           ## "LateMM" = fit_LateMM(silent = TRUE),
           { print(paste0(">> Call to unknown model", tg.model))}
           );
}  ## End of TG.fit_model
################################################################################
source("src/TG_fit_Gamma.R"); source("src/TG_fit_T0Gamma.R");
source("src/TG_fit_GammaInt.R"); source("src/TG_fit_T0GammaInt.R");
################################################################################
TG.parms_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = parms_Gamma(),
           "T0Gamma" = parms_T0Gamma(),
           "GammaInt" = parms_GammaInt(),
           "T0GammaInt" = parms_T0GammaInt(),
           ## "EarlyMM" = parms_EarlyMM(),
           ## "LateExp" = parms_LateExp(),
           ## "LateMM" = parms_LateMM(),
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
        fit_Gamma = TG.fit_Gamma, get_Gamma = TG.get_Gamma, parms_Gamma = TG.parms_Gamma,
        fit_T0Gamma = TG.fit_T0Gamma, get_T0Gamma = TG.get_T0Gamma, parms_T0Gamma = TG.parms_T0Gamma,
        fit_GammaInt = TG.fit_GammaInt, get_GammaInt = TG.get_GammaInt, parms_GammaInt = TG.parms_GammaInt,
        fit_T0GammaInt = TG.fit_T0GammaInt, get_T0GammaInt = TG.get_T0GammaInt, parms_T0GammaInt = TG.parms_T0GammaInt,
        fit_model = TG.fit_model, get_model = TG.get_model, parms_model = TG.parms_model,
        get_model = TG.get_model, plot_fit = TG.plot_fit
    )
);  ## End of TG setRefClass
################################################################################

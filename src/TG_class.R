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
           "Gamma" = get_Gamma_thrombin_int(), "T0Gamma" = get_T0Gamma_thrombin_int(),
           "GammaInt" = get_GammaInt_thrombin_int(),
           "T0GammaInt" = get_T0GammaInt_thrombin_int(),
           "LateExpGammaInt" = get_LateExpGammaInt_thrombin_int(),
           "LateExpT0GammaInt" = get_LateExpT0GammaInt_thrombin_int(),
           { print(paste0(">> Call to unknown get_model ", tg.model))}
           );
}  ## End of TG.get_model
################################################################################

################################################################################
TG.get_A2mT_int <- function(tg.model) {
    switch(tg.model,
           "Gamma" = get_Gamma_A2mT_int(), "T0Gamma" = get_T0Gamma_A2mT_int(),
           "GammaInt" = get_GammaInt_A2mT_int(),
           "T0GammaInt" = get_T0GammaInt_A2mT_int(),
           "LateExpGammaInt" = get_LateExpGammaInt_A2mT_int(),
           "LateExpT0GammaInt" = get_LateExpT0GammaInt_A2mT_int(),
           { print(paste0(">> Call to unknown get_model ", tg.model))}
           );
}  ## End of TG.get_model
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
        parms_Gamma = TG.parms_Gamma, get_Gamma_thrombin_int = TG.get_Gamma_thrombin_int,
        get_Gamma_A2mT_int = TG.get_Gamma_A2mT_int,
        ## T0Gamma
        fit_T0Gamma = TG.fit_T0Gamma, get_T0Gamma = TG.get_T0Gamma,
        parms_T0Gamma = TG.parms_T0Gamma, get_T0Gamma_thrombin_int = TG.get_T0Gamma_thrombin_int,
        get_T0Gamma_A2mT_int = TG.get_T0Gamma_A2mT_int,
        ## GammaInt
        fit_GammaInt = TG.fit_GammaInt, get_GammaInt = TG.get_GammaInt,
        parms_GammaInt = TG.parms_GammaInt, get_GammaInt_thrombin_int = TG.get_GammaInt_thrombin_int,
        get_GammaInt_A2mT_int = TG.get_GammaInt_A2mT_int,
        ## T0GammaInt
        fit_T0GammaInt = TG.fit_T0GammaInt, get_T0GammaInt = TG.get_T0GammaInt,
        parms_T0GammaInt = TG.parms_T0GammaInt, get_T0GammaInt_thrombin_int = TG.get_T0GammaInt_thrombin_int,
        get_T0GammaInt_A2mT_int = TG.get_T0GammaInt_A2mT_int,
        ## LateExpGammaInt
        fit_LateExpGammaInt = TG.fit_LateExpGammaInt, get_LateExpGammaInt = TG.get_LateExpGammaInt,
        parms_LateExpGammaInt = TG.parms_LateExpGammaInt,
        ## LateExpT0GammaInt
        fit_LateExpT0GammaInt = TG.fit_LateExpT0GammaInt, get_LateExpT0GammaInt = TG.get_LateExpT0GammaInt,
        parms_LateExpT0GammaInt = TG.parms_LateExpT0GammaInt,
        ## model
        fit_model = TG.fit_model, get_model = TG.get_model, parms_model = TG.parms_model,
        get_thrombin_int = TG.get_thrombin_int, get_A2mT_int = TG.get_A2mT_int,
        plot_fit = TG.plot_fit
    )
);  ## End of TG setRefClass
################################################################################

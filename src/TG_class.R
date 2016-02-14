source("src/TG_plotting_methods.R");
################################################################################
TG.fit_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = fit_Gamma(silent = FALSE),
           ## "EarlyMM" = fit_EarlyMM(),
           ## "LateExp" = fit_LateExp(),
           ## "LateMM" = fit_LateMM(silent = TRUE),
           { print(paste0(">> Call to unknown model", tg.model))}
           );
}  ## End of TG.fit_model
################################################################################
source("src/TG_fit_Gamma.R");
################################################################################
TG.parms_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = parms_Gamma(),
           ## "EarlyMM" = parms_EarlyMM(),
           ## "LateExp" = parms_LateExp(),
           ## "LateMM" = parms_LateMM(),
           { print(paste0(">> Call to unknown model", tg.model))}
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
        fit_model = TG.fit_model, get_model = TG.get_model, parms_model = TG.parms_model,
        get_model = TG.get_model, plot_fit = TG.plot_fit
    )
);  ## End of TG setRefClass
################################################################################

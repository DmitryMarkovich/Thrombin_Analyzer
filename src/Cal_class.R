################################################################################
source("src/Cal_fit_LM.R");
source("src/Cal_fit_EarlyMM.R");
source("src/Cal_fit_LateExp.R");
source("src/Cal_fit_T0LateExp.R");
source("src/Cal_fit_LateMM.R");
source("src/Cal_fit_T0LateMM.R");
source("src/Cal_fit_Auto.R");
################################################################################
Cal.fit_model <- function(cal.model) {
    switch(cal.model,
           "LM" = fit_LM(silent = TRUE),
           "EarlyMM" = fit_EarlyMM(silent = TRUE),
           "LateExp" = fit_LateExp(silent = TRUE),
           "T0LateExp" = fit_T0LateExp(silent = TRUE),
           "LateMM" = fit_LateMM(silent = TRUE),
           "T0LateMM" = fit_T0LateMM(silent = TRUE),
           "Auto" = fit_Auto(silent = TRUE),
           {
               warning(paste0(">> Call to unknown model", cal.model));
               return(NULL);
           }
           );
}  ## End of Cal.fit_model
################################################################################
Cal.get_model <- function(cal.model) {
    switch(cal.model,
           "LM" = get_LM(),
           "EarlyMM" = get_EarlyMM(),
           "LateExp" = get_LateExp(),
           "T0LateExp" = get_T0LateExp(),
           "LateMM" = get_LateMM(),
           "T0LateMM" = get_T0LateMM(),
           "Auto" = get_Auto(),
           {
               warning(paste0(">> Call to unknown get_model", cal.model));
               return(NULL);
           }
           );
}  ## End of Cal.get_model
################################################################################
################################################################################
Cal.get_init_rate <- function(cal.model) {
    switch(cal.model,
           "LateExp" = get_init_rate_LateExp(),
           "T0LateExp" = get_init_rate_LateExp(),
           "LateMM" = get_init_rate_LateMM(),
           "T0LateMM" = get_init_rate_T0LateMM(),
           "Auto" = get_init_rate_Auto(),
           {
               warning(paste0(">> Call to unknown get_model", cal.model));
               return(rep(0, length(data$x)));
           }
           );
}  ## End of Cal.get_model
################################################################################
source("src/Cal_plotting_methods.R");
################################################################################
Cal.parms_model <- function(cal.model, e0, s0) {
    switch(cal.model,
           "LM" = parms_LM(e0, s0),
           "EarlyMM" = parms_EarlyMM(e0, s0),
           "LateExp" = parms_LateExp(e0, s0),
           "T0LateExp" = parms_LateExp(e0, s0),
           "LateMM" = parms_LateMM(e0, s0),
           "T0LateMM" = parms_T0LateMM(e0, s0),
           "Auto" = parms_Auto(e0, s0),
           {
               warning(paste0(">> Call to unknown model", cal.model));
               return(NULL);
           }
           );
}  ## End of Cal.parms_model
################################################################################

################################################################################
Cal <- setRefClass(
    Class = "Cal", contains = "Base",
    fields = list(
        data = "data.frame", num.smry = "list", fit = "list",
        parms = "data.frame"
    ),
    methods = list(
        clear = Cal.clear, plot = Cal.plot,
        ## LM
        fit_LM = Cal.fit_LM, get_LM = Cal.get_LM, parms_LM = Cal.parms_LM,
        ## EarlyMM
        fit_EarlyMM = Cal.fit_EarlyMM, get_EarlyMM = Cal.get_EarlyMM,
        parms_EarlyMM = Cal.parms_EarlyMM,
        ## LateExp
        fit_LateExp = Cal.fit_LateExp, get_LateExp = Cal.get_LateExp,
        parms_LateExp = Cal.parms_LateExp, get_init_rate_LateExp = Cal.get_init_rate_LateExp,
        ## T0LateExp
        fit_T0LateExp = Cal.fit_T0LateExp, get_T0LateExp = Cal.get_T0LateExp,
        parms_T0LateExp = Cal.parms_T0LateExp, get_init_rate_T0LateExp = Cal.get_init_rate_T0LateExp,
        ## LateMM
        fit_LateMM = Cal.fit_LateMM, get_LateMM = Cal.get_LateMM,
        parms_LateMM = Cal.parms_LateMM, get_init_rate_LateMM = Cal.get_init_rate_LateMM,
        ## T0LateMM
        fit_T0LateMM = Cal.fit_T0LateMM, get_T0LateMM = Cal.get_T0LateMM,
        parms_T0LateMM = Cal.parms_T0LateMM, get_init_rate_T0LateMM = Cal.get_init_rate_T0LateMM,
        ## Auto
        fit_Auto = Cal.fit_Auto, get_Auto = Cal.get_Auto,
        parms_Auto = Cal.parms_Auto,
        ## Model
        fit_model = Cal.fit_model, get_model = Cal.get_model,
        parms_model = Cal.parms_model, get_init_rate = Cal.get_init_rate,
        plot_fit = Cal.plot_fit, plot_residuals = Cal.plot_residuals
    )
);  ## End of Cal setRefClass
################################################################################

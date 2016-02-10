source("src/Cal_plotting_methods.R");
################################################################################
Cal.fit_model <- function(cal.model) {
    switch(cal.model,
           "LM" = fit_LM(),
           "EarlyMM" = fit_EarlyMM(),
           "LateExp" = fit_LateExp(),
           "LateMM" = fit_LateMM(silent = TRUE),
           { print(paste0(">> Call to unknown model", cal.model))}
           );
}  ## End of Cal.fit_model
################################################################################
source("src/Cal_fit_LM.R");
source("src/Cal_fit_EarlyMM.R");
source("src/Cal_fit_LateExp.R");
source("src/Cal_fit_LateMM.R");
################################################################################
Cal.set_e0 <- function(cal.e0) {
    e0 <<- cal.e0;
}  ## End of Cal.set_e0
################################################################################

################################################################################
Cal.set_s0 <- function(cal.s0) {
    s0 <<- cal.s0;
}  ## End of Cal.set_s0
################################################################################

################################################################################
Cal.parms_model <- function(cal.model) {
    switch(cal.model,
           "LM" = parms_LM(),
           "EarlyMM" = parms_EarlyMM(),
           "LateExp" = parms_LateExp(),
           "LateMM" = parms_LateMM(),
           { print(paste0(">> Call to unknown model", cal.model))}
           );
}  ## End of Cal.parms_model
################################################################################

################################################################################
Cal <- setRefClass(
    Class = "Cal", contains = "Base",
    fields = list(
        data = "data.frame", num.smry = "list", fit = "list",
        e0 = "numeric", s0 = "numeric", parms = "data.frame"
    ),
    methods = list(
        clear = Cal.clear, plot = Cal.plot, set_e0 = Cal.set_e0, set_s0 = Cal.set_s0,
        fit_LM = Cal.fit_LM, get_LM = Cal.get_LM, parms_LM = Cal.parms_LM,
        fit_EarlyMM = Cal.fit_EarlyMM, get_EarlyMM = Cal.get_EarlyMM, parms_EarlyMM = Cal.parms_EarlyMM,
        fit_LateExp = Cal.fit_LateExp, get_LateExp = Cal.get_LateExp, parms_LateExp = Cal.parms_LateExp,
        fit_LateMM = Cal.fit_LateMM, get_LateMM = Cal.get_LateMM, parms_LateMM = Cal.parms_LateMM,
        fit_model = Cal.fit_model, get_model = Cal.get_model, parms_model = Cal.parms_model,
        plot_fit = Cal.plot_fit
    )
);  ## End of Cal setRefClass
################################################################################
## print(Cal);

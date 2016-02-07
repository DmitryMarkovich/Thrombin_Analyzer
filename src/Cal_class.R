source("src/Cal_plotting_methods.R");
################################################################################
Cal.fit_model <- function(calModel) {
    switch(calModel,
           "LM" = fit_LM(),
           "EarlyMM" = fit_EarlyMM(),
           "LateExp" = fit_LateExp(),
           "LateMM" = fit_LateMM(),
           { print(paste0(">> Call to unknown model", calModel))}
           );
}  ## End of Cal.fit_model
################################################################################
source("src/Cal_fit_LM.R");
source("src/Cal_fit_EarlyMM.R");
source("src/Cal_fit_LateExp.R");
source("src/Cal_fit_LateMM.R");

################################################################################
Cal <- setRefClass(
    Class = "Cal", contains = "Base",
    fields = list(data = "data.frame", num.smry = "list", fit = "list"),
    methods = list(
        clear = Cal.clear, plot = Cal.plot,
        fit_LM = Cal.fit_LM, get_LM = Cal.get_LM,
        fit_EarlyMM = Cal.fit_EarlyMM, get_EarlyMM = Cal.get_EarlyMM,
        fit_LateExp = Cal.fit_LateExp, get_LateExp = Cal.get_LateExp,
        fit_LateMM = Cal.fit_LateMM, get_LateMM = Cal.get_LateMM,
        fit_model = Cal.fit_model, get_model = Cal.get_model,
        plot_fit = Cal.plot_fit
    )
);  ## End of Cal setRefClass
################################################################################
## print(Cal);

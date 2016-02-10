source("src/TG_plotting_methods.R");
################################################################################
TG <- setRefClass(
    Class = "TG", contains = "Base",
    fields = list(
        data = "data.frame", num.smry = "list", fit = "list",
        parms = "data.frame"
    ),
    methods = list(
        clear = TG.clear, plot = TG.plot, plot_drv1 = TG.plot_drv1
    )
);  ## End of TG setRefClass
################################################################################

################################################################################
Cal <- R6::R6Class(
    classname = "Cal", portable = FALSE, inherit = BaseR6,
    private = list(data = "data.frame", num.smry = "list", fit = "list",
        parms = "data.frame"),
    public = list(
        clear = compiler::cmpfun(
            f = function() {
                data <<- data.frame(); num.smry <<- list(); fit <<- list();
                parms <<- data.frame();
            }, options = kCmpFunOptions)
        )  ## End of public
    );  ## End of Cal
################################################################################

## ################################################################################
## Cal$set(
##     which = "public", name = "clear",
##     value = compiler::cmpfun(
##         f = function() {
##             data <<- data.frame(); num.smry <<- list(); fit <<- list();
##             parms <<- data.frame();
##         }, options = kCmpFunOptions),
##     overwrite = FALSE);  ## End of Cal$clear
## ################################################################################

################################################################################
## Add public methods to Cal class
source("src/Cal_fit_LM.R");
source("src/Cal_fit_EarlyMM.R");
source("src/Cal_fit_LateExp.R");
source("src/Cal_fit_T0LateExp.R");
source("src/Cal_fit_LateMM.R");
source("src/Cal_fit_T0LateMM.R");
source("src/Cal_fit_Auto.R");
source("src/Cal_model_dispatchers.R");
source("src/Cal_plotting_methods.R");
## print(Cal);
################################################################################

################################################################################
######################################## Legacy RF classes code
################################################################################

## ################################################################################
## Cal <- setRefClass(
##     Class = "Cal", contains = "Base",
##     fields = list(
##         data = "data.frame", num.smry = "list", fit = "list",
##         parms = "data.frame"
##     ),
##     methods = list(
##         clear = Cal.clear, plot = Cal.plot,
##         ## LM
##         fit_LM = Cal.fit_LM, get_LM = Cal.get_LM, parms_LM = Cal.parms_LM,
##         ## EarlyMM
##         fit_EarlyMM = Cal.fit_EarlyMM, get_EarlyMM = Cal.get_EarlyMM,
##         parms_EarlyMM = Cal.parms_EarlyMM,
##         ## LateExp
##         fit_LateExp = Cal.fit_LateExp, get_LateExp = Cal.get_LateExp,
##         parms_LateExp = Cal.parms_LateExp, get_init_rate_LateExp = Cal.get_init_rate_LateExp,
##         ## T0LateExp
##         fit_T0LateExp = Cal.fit_T0LateExp, get_T0LateExp = Cal.get_T0LateExp,
##         parms_T0LateExp = Cal.parms_T0LateExp, get_init_rate_T0LateExp = Cal.get_init_rate_T0LateExp,
##         ## LateMM
##         fit_LateMM = Cal.fit_LateMM, get_LateMM = Cal.get_LateMM,
##         parms_LateMM = Cal.parms_LateMM, get_init_rate_LateMM = Cal.get_init_rate_LateMM,
##         ## T0LateMM
##         fit_T0LateMM = Cal.fit_T0LateMM, get_T0LateMM = Cal.get_T0LateMM,
##         parms_T0LateMM = Cal.parms_T0LateMM, get_init_rate_T0LateMM = Cal.get_init_rate_T0LateMM,
##         ## Auto
##         fit_Auto = Cal.fit_Auto, get_Auto = Cal.get_Auto,
##         parms_Auto = Cal.parms_Auto, get_init_rate_Auto = Cal.get_init_rate_Auto,
##         ## Model
##         fit_model = Cal.fit_model, get_model = Cal.get_model,
##         parms_model = Cal.parms_model, get_init_rate = Cal.get_init_rate,
##         plot_fit = Cal.plot_fit, plot_residuals = Cal.plot_residuals
##     )
## );  ## End of Cal setRefClass
## ################################################################################

################################################################################
######################################## End of Legacy RF classes code
################################################################################

################################################################################
Cal <- R6::R6Class(
    classname = "Cal", portable = FALSE, inherit = Base,
    private = list(
        data = data.frame(), num.smry = list(), fit = list(),
        parms = data.frame()
        ),
    public = list(
        clear = compiler::cmpfun(
            f = function() {
                data <<- data.frame(); num.smry <<- list(); fit <<- list();
                parms <<- data.frame();
            }, options = kCmpFunOptions),
        updateProgress = function(progress, amount, detail = NULL) {
            progress$inc(amount = amount, detail = detail);
        }
        )  ## End of public
    );  ## End of Cal
################################################################################

################################################################################
## Add public methods to Cal class
source("src/Cal_fit_LM.R");
source("src/Cal_fit_EarlyMM.R");
source("src/Cal_fit_LateExp.R");
source("src/Cal_fit_T0LateExp.R");
source("src/Cal_fit_LateMM.R");
source("src/Cal_fit_T0LateMM.R");
source("src/Cal_fit_CombinedMM.R");
source("src/Cal_fit_Auto.R");
source("src/Cal_model_dispatchers.R");
source("src/Cal_plotting_methods.R");
################################################################################

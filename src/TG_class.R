################################################################################
TG <- R6::R6Class(
    classname = "TG", portable = FALSE, inherit = Base,
    private = list(
        data = "data.frame", num.smry = "list", num.eval = "list",
        fit = "list", parms = "data.frame"),
    public = list(
        clear = compiler::cmpfun(
            f = function() {
                data <<- data.frame(); num.smry <<- list(); num.eval <<- list();
                fit <<- list(); parms <<- data.frame();
                return(0L);
            }, options = kCmpFunOptions),
        get_data = compiler::cmpfun(
            f = function() {
                return(data);
            }, options = kCmpFunOptions),
        set_data = compiler::cmpfun(
            f = function(df) {
                data <<- df;
                return(0L);
            }, options = kCmpFunOptions),
        get_num_smry = compiler::cmpfun(
            f = function() {
                return(num.smry);
            }, options = kCmpFunOptions),
        set_num_smry = compiler::cmpfun(
            f = function(l) {
                num.smry <<- l;
                return(0L);
            }, options = kCmpFunOptions),
        get_num_eval = compiler::cmpfun(
            f = function() {
                return(num.eval);
            }, options = kCmpFunOptions),
        set_num_eval = compiler::cmpfun(
            f = function(l) {
                num.eval <<- l;
                return(0L);
            }, options = kCmpFunOptions),
        get_fit = compiler::cmpfun(
            f = function() {
                return(fit);
            }, options = kCmpFunOptions),
        set_fit = compiler::cmpfun(
            f = function(l) {
                fit <<- l;
                return(0L);
            }, options = kCmpFunOptions),
        fit_Auto_ok = compiler::cmpfun(
            f = function() {
                return(!is.null(fit$Auto) && fit$Auto);
            }, options = kCmpFunOptions),
        get_parms = compiler::cmpfun(
            f = function() {
                return(parms);
            }, options = kCmpFunOptions),
        num_parms = compiler::cmpfun(
            f = function() {
                return(num.eval$parms);
            }, options = kCmpFunOptions),
        auto_model = compiler::cmpfun(
            f = function() {
                return(fit$Auto_model);
            }, options = kCmpFunOptions),
        is_none_auto_model = compiler::cmpfun(
            f = function() {
                return(!is.null(fit$Auto_model) && fit$Auto_model == "None");
            }, options = kCmpFunOptions),
        is_ok_num_smry = compiler::cmpfun(
            f = function() {
                return(length(num.smry) >= 1 && length(num.smry$drv1) > 1);
            }, options = kCmpFunOptions),
        get_tpeak = compiler::cmpfun(
            f = function(k, theta, t0 = 0) {
                return(t0 + (k - 1) * theta);
            }, options = kCmpFunOptions),
        get_peak = compiler::cmpfun(
            f = function(A, k, theta) {
                return(A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta));
            }, options = kCmpFunOptions),
        get_vel_tpeak = compiler::cmpfun(
            f = function(k, theta, t0 = 0) {
                return(t0 + theta * (k - 1 - sqrt(k - 1)));
            }, options = kCmpFunOptions),
        get_vel_peak = compiler::cmpfun(
            f = function(A, k, theta) {
                return(A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                           exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2));
            }, options = kCmpFunOptions),
        get_parameter = compiler::cmpfun(
            f = function(parameter) {
                if (any(parameter == parms$Parameter)) {
                    return(parms$Value[parms$Parameter == parameter]);
                } else {
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        ## from Rcpp_functions.cpp
        eval_A2mT = eval_A2mT
        )  ## End of public
    );  ## End of TG
################################################################################

################################################################################
TG$set(
    which = "public", name = "evaluate_numerically",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (length(data) != 0) {
                if (is.na(num.smry$t.lin) || num.smry$rat$y <= kYNone) {
                    if (!silent)
                        warning(">> Signal is mostly noise, skipping numerical evaluation!");
                    num.eval <<- list();
                    return(NULL);
                } else {
                    ## print(">> Signal is not noise, do evaluation.");
                    fit.late <- lm(y ~ x,
                                   data = data.frame(x = data$x - num.smry$t.lin,
                                       y = data$y), subset = data$x >= num.smry$t.lin);
                    ## print(summary(fit.late));
                    k <- coef(fit.late)[[2]] / (coef(fit.late)[[1]] - data$y[1]);
                    dt <- data$x[2] - data$x[1];

                    ## A2mT <- rep(0, length(data$x));
                    ## for (i in 2:length(A2mT)) {
                    ##     A2mT[i] <- A2mT[i - 1] + k * 0.5 * dt *
                    ##         (num.smry$drv1[i] + num.smry$drv1[i - 1]);
                    ## }

                    A2mT <- eval_A2mT(num.smry$drv1, k, dt);
                    ## print(A2mT);
                    thromb <- num.smry$drv1 - A2mT;
                    ETP <- dt * (
                        sum(thromb, na.rm = TRUE) -
                            0.5 * (thromb[1] + thromb[length(thromb[!is.na(thromb)])]));
                    ## print(ETP);
                    peak <- max(thromb, na.rm = TRUE)[1];
                    ## print(peak);
                    tt.peak <- data$x[thromb == peak][1];
                    ## print(tt.peak);
                    vel.index <- max(num.smry$drv2, na.rm = TRUE)[1];
                    ## print(vel.index);
                    a2m.level <- max(A2mT, na.rm = TRUE)[1];
                    ## print(a2m.level);
                    lagtime <- data$x[data$x < num.smry$t.peak &
                                          num.smry$drv1 <= 0.1 * peak];
                    lagtime <- lagtime[!is.na(lagtime)]; lagtime <- lagtime[length(lagtime)];
                    if (length(lagtime) == 0) {
                        lagtime <- 0;
                    }
                    ## print(lagtime);
                    ## print(data$x);
                    ## print(num.smry$drv1 - num.eval$k * (data$x[2] - data$x[1]) *
                    ##           cumsum(num.smry$drv1));
                    ## print(lagtime);
                    ## print(ETP);
                    ## print(peak);
                    ## print(tt.peak);
                    ## print(vel.index);
                    ## print(a2m.level);
                    num.eval <<- list(
                        ## a = coef(fit.late)[[1]], b = coef(fit.late)[[2]], k = k, A2mT = A2mT,
                        parms = data.frame(
                            Parameter = kParameterNames,
                            Value = c(lagtime, ETP, peak, tt.peak, vel.index, a2m.level),
                            Units = kAUnits)
                        );
                    if (!silent)
                        print(num.eval$parms);
                }
            } else {
                warning(">> length(data) == 0!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$evaluate_numerically
################################################################################

## source("src/TG_fit_Gamma.R");
## source("src/TG_fit_GammaInt.R");
source("src/TG_fit_T0Gamma.R");
source("src/TG_fit_T0GammaInt.R");
source("src/TG_fit_T0GammaInt2.R");
## source("src/TG_fit_LateExpGammaInt.R");
## source("src/TG_fit_LateExpT0GammaInt.R");
source("src/TG_fit_Auto.R");

source("src/TG_get_thrombin_int.R"); source("src/TG_get_A2mT_int.R");
source("src/TG_get_drv1.R");
source("src/TG_get_thrombin.R"); source("src/TG_get_A2mT.R");
source("src/TG_get_drv2.R");
source("src/TG_get_thrombin_vel.R"); source("src/TG_get_A2mT_vel.R");
source("src/TG_model_dispatchers.R"); source("src/TG_plotting_methods.R");

## print(TG);
## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.clear <- function() {
##     data <<- data.frame(); num.smry <<- list(); num.eval <<- list();
##     fit <<- list(); parms <<- data.frame();
## }  ## End of TG.clear
## ################################################################################

## ################################################################################
## TG.evaluate_numerically <- function(silent = TRUE) {
##     if (length(data) != 0) {
##         if (is.na(num.smry$t.lin) || num.smry$rat$y <= kYNone) {
##             if (!silent)
##                 warning(">> Signal is mostly noise, skipping numerical evaluation!");
##             num.eval <<- list();
##             return(NULL);
##         } else {
##             fit.late <- lm(y ~ x, data = data.frame(x = data$x - num.smry$t.lin,
##                                       y = data$y),
##                            subset = data$x >= num.smry$t.lin);
##             ## print(summary(fit.late));
##             k <- coef(fit.late)[[2]] / (coef(fit.late)[[1]] - data$y[1]);
##             dt <- data$x[2] - data$x[1];
##             ## A2mT <- rep(0, length(data$x)); 
##             ## for (i in 2:length(A2mT)) {
##             ##     A2mT[i] <- A2mT[i - 1] + k * 0.5 * dt *
##             ##         (num.smry$drv1[i] + num.smry$drv1[i - 1]);
##             ## }
##             A2mT <- eval_A2mT(num.smry$drv1, k, dt);
##             ## print(A2mT);
##             thromb <- num.smry$drv1 - A2mT;
##             lagtime <- data$x[data$x < num.smry$t.peak &
##                                   num.smry$drv1 <= 0.1 * data$y[1]];
##             lagtime <- lagtime[!is.na(lagtime)]; lagtime <- lagtime[length(lagtime)];
##             ## print(lagtime);
##             ETP <- dt * (
##                 sum(thromb, na.rm = TRUE) -
##                     0.5 * (thromb[1] + thromb[length(thromb[!is.na(thromb)])]));
##             ## print(ETP);
##             peak <- max(thromb, na.rm = TRUE)[1];
##             ## print(peak);
##             tt.peak <- data$x[thromb == peak][1];
##             ## print(tt.peak);
##             vel.index <- max(num.smry$drv2[data$x <= tt.peak], na.rm = TRUE)[1];
##             ## print(vel.index);
##             a2m.level <- max(A2mT, na.rm = TRUE)[1];
##             ## print(a2m.level);
##             ## print(data$x);
##             ## print(num.smry$drv1 - num.eval$k * (data$x[2] - data$x[1]) *
##             ##           cumsum(num.smry$drv1));
##             num.eval <<- list(
##                 ## a = coef(fit.late)[[1]], b = coef(fit.late)[[2]], k = k, A2mT = A2mT,
##                 parms = data.frame(
##                     Parameter = kParameterNames,
##                     Value = c(lagtime, ETP, peak, tt.peak, vel.index, a2m.level),
##                     Units = kAUnits)
##                 );
##             if (!silent)
##                 print(num.eval$parms);
##         }
##     } else {
##         warning(">> length(data) == 0!");
##         return(NULL);
##     }
## }  ## End of TG.evaluate_numerically
## ################################################################################

## ################################################################################
## TG <- setRefClass(
##     Class = "TG", contains = "Base",
##     fields = list(
##         data = "data.frame", num.smry = "list", num.eval = "list",
##         fit = "list", parms = "data.frame"
##     ),
##     methods = list(
##         clear = TG.clear, evaluate_numerically = TG.evaluate_numerically,
##         plot = TG.plot, plot_drv1 = TG.plot_drv1, plot_drv2 = TG.plot_drv2,
##         ## getter functions
##         get_tpeak = GetTPeak, get_peak = GetPeak,
##         get_vel_tpeak = GetVelTPeak, get_vel_peak = GetVelPeak,
##         ## Gamma
##         fit_Gamma = TG.fit_Gamma, get_Gamma = TG.get_Gamma,
##         parms_Gamma = TG.parms_Gamma,
##         ## T0Gamma
##         fit_T0Gamma = TG.fit_T0Gamma, get_T0Gamma = TG.get_T0Gamma,
##         parms_T0Gamma = TG.parms_T0Gamma,
##         ## GammaInt
##         fit_GammaInt = TG.fit_GammaInt, get_GammaInt = TG.get_GammaInt,
##         parms_GammaInt = TG.parms_GammaInt,
##         ## T0GammaInt
##         fit_T0GammaInt = TG.fit_T0GammaInt, get_T0GammaInt = TG.get_T0GammaInt,
##         parms_T0GammaInt = TG.parms_T0GammaInt,
##         ## T0GammaInt2
##         fit_T0GammaInt2 = TG.fit_T0GammaInt2, get_T0GammaInt2 = TG.get_T0GammaInt2,
##         parms_T0GammaInt2 = TG.parms_T0GammaInt2,
##         ## LateExpGammaInt
##         fit_LateExpGammaInt = TG.fit_LateExpGammaInt,
##         get_LateExpGammaInt = TG.get_LateExpGammaInt,
##         parms_LateExpGammaInt = TG.parms_LateExpGammaInt,
##         ## LateExpT0GammaInt
##         fit_LateExpT0GammaInt = TG.fit_LateExpT0GammaInt,
##         get_LateExpT0GammaInt = TG.get_LateExpT0GammaInt,
##         parms_LateExpT0GammaInt = TG.parms_LateExpT0GammaInt,
##         ## Auto
##         fit_Auto = TG.fit_Auto, get_Auto = TG.get_Auto,
##         parms_Auto = TG.parms_Auto,
##         compare_T0GammaInt2_and_T0GammaInt = TG.compare_T0GammaInt2_and_T0GammaInt,
##         compare_T0GammaInt_and_T0Gamma = TG.compare_T0GammaInt_and_T0Gamma,
##         ## model
##         fit_model = TG.fit_model, get_model = TG.get_model,
##         parms_model = TG.parms_model,
##         get_thrombin_int = TG.get_thrombin_int,
##         get_thrombin_int_contribution = TG.get_thrombin_int_contribution,
##         get_A2mT_int = TG.get_A2mT_int,
##         get_thrombin = TG.get_thrombin,
##         get_thrombin_contribution = TG.get_thrombin_contribution,
##         get_A2mT = TG.get_A2mT,
##         get_thrombin_vel = TG.get_thrombin_vel, get_A2mT_vel = TG.get_A2mT_vel,
##         get_thrombin_vel_contribution = TG.get_thrombin_vel_contribution,
##         get_drv1 = TG.get_drv1, get_drv2 = TG.get_drv2,
##         plot_fit = TG.plot_fit, plot_residuals = TG.plot_residuals,
##         plot_thrombogram = TG.plot_thrombogram,
##         plot_velocity = TG.plot_velocity
##     )
## );  ## End of TG setRefClass
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

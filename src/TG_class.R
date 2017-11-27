################################################################################
TG <- R6::R6Class(
    classname = "TG", portable = FALSE, inherit = Base,
    private = list(
        data = data.frame(), num.smry = list(), num.eval = list(),
        fit = list(), parms = data.frame()
        ),
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
        get_vel_tmin = compiler::cmpfun(
            f = function(k, theta, t0 = 0) {
                return(t0 + theta * (k - 1 + sqrt(k - 1)));
            }, options = kCmpFunOptions),
        get_vel_peak = compiler::cmpfun(
            f = function(A, k, theta) {
                return(A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                           exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2));
            }, options = kCmpFunOptions),
        tpeak_est_f = compiler::cmpfun(
            f = function(t, A1, A2, k1, k2, theta, t0) {
                return(
                (A1 * (t - t0) ^ (k1 - 1) * (((k1 - 1) / (t - t0)) - (1 / theta))) /
                (gamma(k1) * theta ^ (k1)) +
                (A2 * (t - t0) ^ (k2 - 1) * (((k2 - 1) / (t - t0)) - (1 / theta))) /
                (gamma(k2) * theta ^ (k2))
                );
            }, options = kCmpFunOptions),
        tpeak_est = compiler::cmpfun(
            f = function(A1, A2, k1, k2, theta, t0, t.peak1, t.peak2) {
                t1 <- min(c(t.peak1, t.peak2));
                t2 <- max(c(t.peak1, t.peak2));
                res <- uniroot(f = tpeak_est_f, interval = c(t1, t2),
                               A1 = A1, A2 = A2, k1 = k1, k2 = k2,
                               theta = theta, t0 = t0,
                               f.lower = tpeak_est_f(t = t1, A1 = A1, A2 = A2,
                                                     k1 = k1, k2 = k2,
                                                     theta = theta, t0 = t0),
                               f.upper = tpeak_est_f(t = t2, A1 = A1, A2 = A2,
                                                     k1 = k1, k2 = k2,
                                                     theta = theta, t0 = t0),
                               trace = 1);
                return(res$root);
            }, options = kCmpFunOptions),
        peak_est = compiler::cmpfun(
            f = function(model, t.peak) {
                return(
                    get_thrombin_contribution(model, number = 1, time = t.peak) +
                    get_thrombin_contribution(model, number = 2, time = t.peak)
                );
            }, options = kCmpFunOptions),
        get_velocity = compiler::cmpfun(
            f = function(time, t0, A, k, theta) {
                v <- A * exp(-(time - t0) / theta) *
                    (time - t0) ^ (k - 1) *
                                      ((-1 / theta) +
                                       ((k - 1) / (time - t0))) /
                                      (gamma(k) * theta ^ k);
                v[time <= t0] <- 0;
                return(v);
            }, options = kCmpFunOptions),
        check_tvelpeak = compiler::cmpfun(
            f = function(t.vel.peak1, t.vel.peak2, t.peak) {
                if (t.vel.peak1 > t.peak) {
                    return(t.vel.peak2);
                } else if (t.vel.peak2 > t.peak) {
                    return(t.vel.peak1);
                } else {
                    return(NA_real_);
                }
            }, options = kCmpFunOptions),
        tvelpeak_est_f = compiler::cmpfun(
            f = function(t, A1, A2, k1, k2, theta, t0) {
                return(
                (A1 * (t - t0) ^ (k1 - 1) * (
                    (((k1 - 1) / (t - t0)) - (1 / theta)) ^ 2 -
                                                            ((k1 - 1) / ((t - t0) ^ 2))
                )) / (gamma(k1) * theta ^ (k1)) +
                (A2 * (t - t0) ^ (k2 - 1) * (
                    (((k2 - 1) / (t - t0)) - (1 / theta)) ^ 2 -
                                                            ((k2 - 1) / ((t - t0) ^ 2))
                )) / (gamma(k2) * theta ^ (k2))
                );
            }, options = kCmpFunOptions),
        tvelpeak_est = compiler::cmpfun(
            f = function(A1, A2, k1, k2, theta, t0, t.vel.peak1, t.vel.peak2) {
                t1 <- min(c(t.vel.peak1, t.vel.peak2));
                t2 <- max(c(t.vel.peak1, t.vel.peak2));
                res <- uniroot(f = tvelpeak_est_f, interval = c(t1, t2),
                               A1 = A1, A2 = A2, k1 = k1, k2 = k2,
                               theta = theta, t0 = t0,
                               f.lower = tvelpeak_est_f(t = t1, A1 = A1, A2 = A2,
                                                        k1 = k1, k2 = k2,
                                                        theta = theta, t0 = t0),
                               f.upper = tvelpeak_est_f(t = t2, A1 = A1, A2 = A2,
                                                        k1 = k1, k2 = k2,
                                                        theta = theta, t0 = t0),
                               trace = 1, extendInt = "yes");
                return(res$root);
            }, options = kCmpFunOptions),
        velpeak_est = compiler::cmpfun(
            f = function(model, t.vel.peak) {
                return(
                    get_thrombin_vel_contribution(model, number = 1,
                                                  time = t.vel.peak) +
                    get_thrombin_vel_contribution(model, number = 2,
                                                  time = t.vel.peak)
                );
            }, options = kCmpFunOptions),
        get_parameter = compiler::cmpfun(
            f = function(parameter) {
                if (any(parameter == parms$Parameter)) {
                    return(parms$Value[parms$Parameter == parameter]);
                } else {
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        get_sigma_auto = compiler::cmpfun(
            f = function() {
                return(fit[[fit$Auto_model]]$smry$sigma);
            }, options = kCmpFunOptions),
        need_substrate_consumption = compiler::cmpfun(
            f = function(ft, ft.sc) {
                if (!is.null(ft) && !is.null(ft.sc)) {
                    ## if fit with sc decreases ft$smry$sigma by a factor larger
                    ## than kSCRatio, sc fit is preffered
                    if (ft$smry$sigma >=
                        kSCRatio * ft.sc$smry$sigma) {
                        ## print(paste0(">> sigma ft = ", ft$smry$sigma,
                        ##              " >= sigma ft.sc * ", kSCRatio, " = ",
                        ##              kSCRatio * ft.sc$smry$sigma));
                        return(TRUE);
                    } else {
                        ## print(paste0(">> sigma ft = ", ft$smry$sigma,
                        ##              " <= sigma ft.sc * ", kSCRatio, " = ",
                        ##              kSCRatio * ft.sc$smry$sigma));
                        return(FALSE);
                    }
                } else {
                    return(NA);
                }
            }, options = kCmpFunOptions),
        updateProgress = function(progress, amount, detail = NULL) {
            progress$inc(amount = amount, detail = detail);
        },
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
                                                     y = data$y),
                                   subset = data$x >= num.smry$t.lin);
                    ## print(summary(fit.late));
                    ETP <- coef(fit.late)[[1]]; a2m.level <- coef(fit.late)[[2]];
                    k <- a2m.level / ETP;
                    ## dt <- data$x[2] - data$x[1];

                    ## A2mT <- rep(0, length(data$x));
                    ## for (i in 2:length(A2mT)) {
                    ##     A2mT[i] <- A2mT[i - 1] + k * 0.5 * dt *
                    ##         (num.smry$drv1[i] + num.smry$drv1[i - 1]);
                    ## }
                    ## A2mT <- 0;
                    ## A2mT <- eval_A2mT(num.smry$drv1, k, dt);
                    ## print(A2mT);
                    thromb <- num.smry$drv1 - k * data$y;
                    ## ETP <- dt * (
                    ##     sum(thromb, na.rm = TRUE) -
                    ##     0.5 * (thromb[1] + thromb[length(thromb[!is.na(thromb)])]));
                    ## ETP <- -1;
                    ## print(ETP);
                    peak <- max(thromb, na.rm = TRUE)[1]; #peak = -1;
                    ## print(peak);
                    ## tt.peak <- data$x[thromb == peak][1]; #tt.peak = -1;
                    ## print(tt.peak);
                    vel.index <- max(num.smry$drv2[data$x <= num.smry$t.peak],
                                     na.rm = TRUE)[1];  ## vel.index = -1;
                    ## print(vel.index);
                    ## a2m.level <- mean(k * data$y[data$x >= num.smry$t.lin],
                    ##                   na.rm = TRUE)[1]; ## a2m.level = -1;

                    ## print(a2m.level);
                    lagtime <- data$x[data$x < num.smry$t.peak &
                                      num.smry$drv1 <= 0.1 * peak];
                    ## lagtime = -1;
                    lagtime <- lagtime[complete.cases(lagtime)];
                    lagtime <- lagtime[length(lagtime)];
                    if (length(lagtime) == 0) {
                        lagtime <- 0;
                    }
                    ## print(lagtime);
                    ## print(data$x);
                    ## print(num.smry$drv1 - num.eval$k * (data$x[2] - data$x[1]) *
                    ##           cumsum(num.smry$drv1));

                    ## print(paste0(">> lagtime = ", lagtime));
                    ## print(paste0(">> ETP = ", ETP));
                    ## print(paste0(">> peak = ", peak));
                    ## print(paste0(">> tt.peak = ", num.smry$t.peak));
                    ## print(paste0(">> vel.index = ", vel.index));
                    ## print(paste0(">> a2m.level = ", a2m.level));
                    num.eval <<- list(
                        parms = data.frame(
                            Parameter = kParameterNames,
                            Value = c(lagtime, ETP, peak, num.smry$t.peak,
                                      vel.index, a2m.level),
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
source("src/TG_fit_T0GammaInt2_test.R");
## source("src/TG_fit_LateExpGammaInt.R");
source("src/TG_fit_LateExpT0GammaInt.R");
source("src/TG_fit_LateExpT0GammaInt2.R");
source("src/TG_fit_Auto.R");
source("src/TG_fit_Pade.R");

source("src/TG_get_thrombin_int.R"); source("src/TG_get_A2mT_int.R");
source("src/TG_get_drv1.R");
source("src/TG_get_thrombin.R"); source("src/TG_get_A2mT.R");
source("src/TG_get_drv2.R");
source("src/TG_get_thrombin_vel.R"); source("src/TG_get_A2mT_vel.R");
source("src/TG_model_dispatchers.R"); source("src/TG_plotting_methods.R");

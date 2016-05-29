################################################################################
TG.clear <- function() {
    data <<- data.frame(); num.smry <<- list(); fit <<- list();
    parms <<- data.frame();
}  ## End of TG.clear
################################################################################

################################################################################
TG.evaluate_numerically <- function() {
    if (length(data) != 0) {
        if (is.na(num.smry$t.lin) || num.smry$rat$y <= 5) {
            warning(">> Signal is mostly noise, skipping numerical evaluation!");
            return(NULL);
        } else {
            fit.late <- lm(y ~ x, data = data.frame(x = data$x - num.smry$t.lin,
                                      y = data$y),
                           subset = data$x >= num.smry$t.lin);
            ## print(summary(fit.late));
            k <- coef(fit.late)[[2]] / (coef(fit.late)[[1]] - data$y[1]);
            A2mT <- rep(0, length(data$x)); delta.t <- data$x[2] - data$x[1];
            for (i in 2:length(A2mT)) {
                A2mT[i] <- A2mT[i - 1] + k * 0.5 * delta.t *
                    (num.smry$drv1[i] + num.smry$drv1[i - 1]);
            }
            ## print(A2mT);
            thromb <- num.smry$drv1 - A2mT;
            lagtime <- data$x[data$x < num.smry$t.peak &
                                  num.smry$drv1 <= 0.1 * data$y[1]];
            lagtime <- lagtime[!is.na(lagtime)]; lagtime <- lagtime[length(lagtime)];
            ## print(lagtime);
            ETP <- delta.t * (
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
            ## print(data$x);
            ## print(num.smry$drv1 - num.eval$k * (data$x[2] - data$x[1]) *
            ##           cumsum(num.smry$drv1));
            num.eval <<- list(
                a = coef(fit.late)[[1]], b = coef(fit.late)[[2]], k = k, A2mT = A2mT,
                parms = data.frame(
                    Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                        "Alpha2M_Level"),
                    Value = c(lagtime, ETP, peak, tt.peak, vel.index, a2m.level),
                    Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                        "a.u. / min"))
                );
            print(num.eval$parms);
        }
    } else {
        warning(">> length(data) == 0!");
        return(NULL);
    }
}  ## End of TG.evaluate_numerically
################################################################################
source("src/TG_fit_Gamma.R");
source("src/TG_fit_GammaInt.R");
source("src/TG_fit_T0Gamma.R");
source("src/TG_fit_T0GammaInt.R");
source("src/TG_fit_T0GammaInt2.R");
source("src/TG_fit_LateExpGammaInt.R");
source("src/TG_fit_LateExpT0GammaInt.R");
source("src/TG_fit_Auto.R");
################################################################################
TG.fit_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = fit_Gamma(silent = TRUE),
           "T0Gamma" = fit_T0Gamma(silent = TRUE),
           "GammaInt" = fit_GammaInt(silent = TRUE),
           "T0GammaInt" = fit_T0GammaInt(silent = TRUE),
           "T0GammaInt2" = fit_T0GammaInt2(silent = TRUE),
           "LateExpGammaInt" = fit_LateExpGammaInt(silent = TRUE),
           "LateExpT0GammaInt" = fit_LateExpT0GammaInt(silent = TRUE),
           "Auto" = fit_Auto(silent = TRUE),
           { warning(paste0(">> Call to unknown model", tg.model))}
           );
}  ## End of TG.fit_model
################################################################################
################################################################################
TG.get_model <- function(tg.model) {
    switch(tg.model,
           "Gamma" = get_Gamma(), "T0Gamma" = get_T0Gamma(),
           "GammaInt" = get_GammaInt(), "T0GammaInt" = get_T0GammaInt(),
           "T0GammaInt2" = get_T0GammaInt2(),
           "LateExpGammaInt" = get_LateExpGammaInt(),
           "LateExpT0GammaInt" = get_LateExpT0GammaInt(),
           "Auto" = get_Auto(),
           { warning(paste0(">> Call to unknown get_model ", tg.model))}
           );
}  ## End of TG.get_model
################################################################################
source("src/TG_get_thrombin_int.R"); source("src/TG_get_A2mT_int.R");
source("src/TG_get_drv1.R");
source("src/TG_get_thrombin.R"); source("src/TG_get_A2mT.R");
source("src/TG_get_drv2.R");
source("src/TG_get_thrombin_vel.R"); source("src/TG_get_A2mT_vel.R");
source("src/TG_plotting_methods.R");
################################################################################
TG.parms_model <- function(tg.model, cal.CF) {
    switch(tg.model,
           "Gamma" = parms_Gamma(cal.CF),
           "T0Gamma" = parms_T0Gamma(cal.CF),
           "GammaInt" = parms_GammaInt(cal.CF),
           "T0GammaInt" = parms_T0GammaInt(cal.CF),
           "T0GammaInt2" = parms_T0GammaInt2(cal.CF),
           "LateExpGammaInt" = parms_LateExpGammaInt(cal.CF),
           "LateExpT0GammaInt" = parms_LateExpT0GammaInt(cal.CF),
           "Auto" = parms_Auto(cal.CF),
           { warning(paste0(">> Call to unknown model ", tg.model))}
           );
}  ## End of TG.parms_model
################################################################################

################################################################################
TG <- setRefClass(
    Class = "TG", contains = "Base",
    fields = list(
        data = "data.frame", num.smry = "list", num.eval = "list",
        fit = "list", parms = "data.frame"
    ),
    methods = list(
        clear = TG.clear, evaluate_numerically = TG.evaluate_numerically,
        plot = TG.plot, plot_drv1 = TG.plot_drv1, plot_drv2 = TG.plot_drv2,
        ## Gamma
        fit_Gamma = TG.fit_Gamma, get_Gamma = TG.get_Gamma,
        parms_Gamma = TG.parms_Gamma,
        ## T0Gamma
        fit_T0Gamma = TG.fit_T0Gamma, get_T0Gamma = TG.get_T0Gamma,
        parms_T0Gamma = TG.parms_T0Gamma,
        ## GammaInt
        fit_GammaInt = TG.fit_GammaInt, get_GammaInt = TG.get_GammaInt,
        parms_GammaInt = TG.parms_GammaInt,
        ## T0GammaInt
        fit_T0GammaInt = TG.fit_T0GammaInt, get_T0GammaInt = TG.get_T0GammaInt,
        parms_T0GammaInt = TG.parms_T0GammaInt,
        ## T0GammaInt2
        fit_T0GammaInt2 = TG.fit_T0GammaInt2, get_T0GammaInt2 = TG.get_T0GammaInt2,
        parms_T0GammaInt2 = TG.parms_T0GammaInt2,
        ## LateExpGammaInt
        fit_LateExpGammaInt = TG.fit_LateExpGammaInt,
        get_LateExpGammaInt = TG.get_LateExpGammaInt,
        parms_LateExpGammaInt = TG.parms_LateExpGammaInt,
        ## LateExpT0GammaInt
        fit_LateExpT0GammaInt = TG.fit_LateExpT0GammaInt,
        get_LateExpT0GammaInt = TG.get_LateExpT0GammaInt,
        parms_LateExpT0GammaInt = TG.parms_LateExpT0GammaInt,
        ## Auto
        fit_Auto = TG.fit_Auto, get_Auto = TG.get_Auto,
        parms_Auto = TG.parms_Auto,
        compare_T0GammaInt2_and_T0GammaInt = TG.compare_T0GammaInt2_and_T0GammaInt,
        ## model
        fit_model = TG.fit_model, get_model = TG.get_model,
        parms_model = TG.parms_model,
        get_thrombin_int = TG.get_thrombin_int,
        get_thrombin_int_contribution = TG.get_thrombin_int_contribution,
        get_A2mT_int = TG.get_A2mT_int,
        get_thrombin = TG.get_thrombin,
        get_thrombin_contribution = TG.get_thrombin_contribution,
        get_A2mT = TG.get_A2mT,
        get_thrombin_vel = TG.get_thrombin_vel, get_A2mT_vel = TG.get_A2mT_vel,
        get_drv1 = TG.get_drv1, get_drv2 = TG.get_drv2,
        plot_fit = TG.plot_fit, plot_residuals = TG.plot_residuals,
        plot_thrombogram = TG.plot_thrombogram,
        plot_velocity = TG.plot_velocity
    )
);  ## End of TG setRefClass
################################################################################

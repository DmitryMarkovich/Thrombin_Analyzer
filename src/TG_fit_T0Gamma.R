################################################################################
TG$set(
    which = "public", name = "fit_T0Gamma",
    value = compiler::cmpfun(
        f = suppressWarnings(function(silent = TRUE) {
            if (!is.null(fit$T0Gamma)) {  ## exists(x = "T0Gamma", where = fit, envir = environment())
                warning(">> No fitting: T0Gamma fit already exists!");
                return(fit$T0Gamma);
            } else {
                start.list <- list(b = data$y[1], A = 0.5 * num.smry$ampl,
                                   k = 3, theta = num.smry$t.peak / 2, t0 = 0);
                ft <- NULL; n.try <- 1;
                while (is.null(ft) && n.try <= kNumTries) {
                    try(expr = {
                            ft <- suppressWarnings(minpack.lm::nlsLM(
                                y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta),
                                data = data, start = start.list, trace = F,
                                ## lower = c(b.min, A.min, k.min, theta.min, t0.min),
                                ## upper = c(b.max, A.max, k.max, theta.max, t0.max),
                                lower = c(  0,   0, 1.25,   0,   0),
                                upper = c(Inf, Inf,  Inf, Inf, Inf),
                                algorithm = "LM",
                                control = minpack.lm::nls.lm.control(
                                    ftol = sqrt(.Machine$double.eps),
                                    ptol = sqrt(.Machine$double.eps),
                                    gtol = 0, factor = 100,  ## between [0.1, 100]
                                    maxiter = 200, nprint = -1
                                    )
                                ))
                        }, silent = TRUE);
                    n.try <- n.try + 1;
                    start.list <- list(b = data[[2]][1], A = runif(1) * num.smry$ampl,
                                       k = runif(1, 1, 10), theta = runif(1, 1, 30),
                                       t0 = 0);
                }
                if (is.null(ft)) {
                    warning(">> fit_T0Gamma resulted in NULL!");
                    return(NULL);
                } else {
                    fit$T0Gamma <<- list(
                        cff = coef(ft), smry = get_compact_summary(ft),  ## summary(ft),
                        diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
                        );
                    if (!silent)
                        print(fit[names(fit) != "LM"]);
                    return(fit$T0Gamma);
                }  ## End of if is.null(fit)
            }  ## End of if exists()
        }), options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$fit_T0Gamma
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_T0Gamma",
    value = compiler::cmpfun(
        f = function() {
            if (!is.null(fit$T0Gamma)) {  ## exists(x = "T0Gamma", where = fit)
                return(fit$T0Gamma$cff[[1]] + fit$T0Gamma$cff[[2]] *
                           pgamma(q = data$x - fit$T0Gamma$cff[["t0"]],
                                  shape = fit$T0Gamma$cff[[3]],
                                  scale = fit$T0Gamma$cff[[4]]));
            } else {
                warning(">> fit$T0Gamma does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_T0Gamma
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_T0Gamma",
    value = compiler::cmpfun(
        f = function(cal.CF) {
            ## print(">> Call to TG.parms_T0Gamma");
            ## if (exists(x = "T0Gamma", where = fit)) {  ## , envir = environment()
            if (!is.null(fit$T0Gamma)) {
                A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
                theta <- fit$T0Gamma$cff[["theta"]];
                t0 <- fit$T0Gamma$cff[["t0"]];
                if (k > 2) {
                    v <- get_vel_peak(A, k, theta);  ## A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                        ## exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
                } else {
                    if (!is.null(num.smry$drv2)) {
                        v <- max(num.smry$drv2, na.rm = TRUE);
                    } else {
                        v <- NA;
                    }
                }
                if (cal.CF != 1) {
                    CF <- cal.CF;
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            CF * A,
                            ## CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                            CF * get_peak(A, k, theta),
                            ## t0 + theta * (k - 1),
                            get_tpeak(k, theta, t0),
                            CF * v,
                            0),
                        Units = kUnits));
                } else {
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            A,
                            ## A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
                            get_peak(A, k, theta),
                            ## t0 + theta * (k - 1),
                            get_tpeak(k, theta, t0),
                            v,
                            0),
                        Units = kAUnits));
                }
            } else {
                warning(">> fit$T0Gamma does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG.parms_T0Gamma
################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.fit_T0Gamma <- function(silent = TRUE) {
##     if (exists(x = "T0Gamma", where = fit)) {
##         warning(">> No fitting: T0Gamma fit already exists!");
##         return(fit$T0Gamma);
##     } else {
##         if (exists(x = "Gamma", where = fit)) {
##             ## use existing Gamma fit for estimates of all parameters except t0
##             start.list <- list(b = fit$Gamma$cff[["b"]], A = fit$Gamma$cff[["A"]],
##                                k = fit$Gamma$cff[["k"]], theta = fit$Gamma$cff[["theta"]],
##                                t0 = 0);
##         } else {
##             ## call fit_Gamma for estimate all parameters except t0
##             ## fit_Gamma(silent = TRUE);
##             ## start.list <- list(b = fit$Gamma$cff[["b"]], A = fit$Gamma$cff[["A"]],
##             ##                    k = fit$Gamma$cff[["k"]], theta = fit$Gamma$cff[["theta"]],
##             ##                    t0 = 0);
##             start.list <- list(b = data$y[1], A = 0.5 * num.smry$ampl,
##                                k = 3, theta = num.smry$t.peak / 2, t0 = 0);
##         }
##         ft <- NULL; n.try <- 1;
##         while (is.null(ft) && n.try <= kNumTries) {
##             try(expr = {
##                 ft <- suppressWarnings(nlsLM(
##                     y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta),
##                     data = data, start = start.list, trace = F,
##                     ## lower = c(b.min, A.min, k.min, theta.min, t0.min),
##                     ## upper = c(b.max, A.max, k.max, theta.max, t0.max),
##                     lower = c(  0,   0, 1.5,   0,   0),
##                     upper = c(Inf, Inf, Inf, Inf, Inf),
##                     algorithm = "LM",
##                     control = nls.lm.control(
##                         ftol = sqrt(.Machine$double.eps),
##                         ptol = sqrt(.Machine$double.eps),
##                         gtol = 0, factor = 100,  ## between [0.1, 100]
##                         maxiter = 200, nprint = -1
##                     )
##                 ))
##             }, silent = FALSE);
##             n.try <- n.try + 1;
##             start.list <- list(b = data[[2]][1], A = runif(1) * num.smry$ampl,
##                                k = runif(1, 1, 10), theta = runif(1, 1, 30),
##                                t0 = 0);
##         }
##         if (is.null(ft)) {
##             warning(">> fit_T0Gamma resulted in NULL!");
##             return(NULL);
##         } else {
##             fit$T0Gamma <<- list(
##                 cff = coef(ft), smry = summary(ft),
##                 diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
##             );
##             if (!silent)
##                 print(fit[names(fit) != "LM"]);
##             return(fit$T0Gamma);
##         }  ## End of if is.null(fit)
##     }  ## End of if exists()
## }  ## End of TG_fitT0Gamma
## ################################################################################

## ################################################################################
## TG.get_T0Gamma <- function() {
##     if (exists(x = "T0Gamma", where = fit)) {
##         return(fit$T0Gamma$cff[[1]] + fit$T0Gamma$cff[[2]] *
##                    pgamma(q = data$x - fit$T0Gamma$cff[["t0"]],
##                           shape = fit$T0Gamma$cff[[3]],
##                           scale = fit$T0Gamma$cff[[4]]));
##     } else {
##         warning(">> fit$T0Gamma does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of TG_get_T0Gamma
## ################################################################################

## ################################################################################
## TG.parms_T0Gamma <- function(cal.CF) {
##     ## print(">> Call to TG.parms_T0Gamma");
##     if (exists(x = "T0Gamma", where = fit)) {
##         A <- fit$T0Gamma$cff[["A"]]; k <- fit$T0Gamma$cff[["k"]];
##         theta <- fit$T0Gamma$cff[["theta"]];
##         t0 <- fit$T0Gamma$cff[["t0"]];
##         if (k > 2) {
##             v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
##                 exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
##         } else {
##             v <- max(num.smry$drv2, na.rm = TRUE);
##         }
##         if (cal.CF != 1) {
##             CF <- cal.CF;
##             return(parms <<- data.frame(
##                 Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
##                     "Alpha2M_Level"),
##                 Value = c(
##                     t0,
##                     CF * A,
##                     CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     CF * v,
##                     0),
##                 ## StdErr = rep(NA, 5),
##                 Units = c("min", "nM * min", "nM", "min", "nM / min", "nM"))
##                    );
##         } else {
##             return(parms <<- data.frame(
##                 Parameter = c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
##                     "Alpha2M_Level"),
##                 Value = c(
##                     t0,
##                     A,
##                     A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     v,
##                     0),
##                 ## StdErr = rep(NA, 5),
##                 Units = c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
##                     "a.u. / min"))
##                    );
##         }
##     } else {
##         warning(">> fit$T0Gamma does not exist!");
##         return(NULL);
##     }
## }  ## End of TG.parms_T0Gamma
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

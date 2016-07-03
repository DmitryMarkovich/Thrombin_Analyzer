################################################################################
TG$set(
    which = "public", name = "fit_T0GammaInt2",
    value = compiler::cmpfun(
        f = suppressWarnings(function(silent = TRUE) {
            if (!silent)
                print(">> fit_T0GammaInt2 called!");
            if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit, envir = environment())
                print(">> No fitting: T0GammaInt2 fit already exists!");
                return(fit$T0GammaInt2);
            } else {
                if (is.null(fit$T0GammaInt)) {  ##!exists(x = "T0GammaInt", where = fit, envir = environment())
                    fit_T0GammaInt(silent = TRUE);
                }
                ## print(fit$T0GammaInt$smry);
                if (!is.null(fit$T0GammaInt)) {
                    start.list <- list(
                        b = fit$T0GammaInt$cff[["b"]],
                        A1 = 0.5 * fit$T0GammaInt$cff[["A"]],
                        A2 = 0.5 * fit$T0GammaInt$cff[["A"]],
                        k1 = fit$T0GammaInt$cff[["k"]], k2 = fit$T0GammaInt$cff[["k"]],
                        theta = fit$T0GammaInt$cff[["theta"]],
                        k.a2m = fit$T0GammaInt$cff[["k.a2m"]],
                        t0 = fit$T0GammaInt$cff[["t0"]]
                        );
                } else {
                    start.list <- list(
                        b = data$y[[1]],
                        A1 = 0.5 * num.smry$ampl,
                        A2 = 0.5 * num.smry$ampl,
                        k1 = 3, k2 = 3,
                        theta = 2,
                        k.a2m = 1e-3,
                        t0 = 0
                        );
                }
                if (start.list$k1 < 2) {
                    start.list$k1 <- 2.5;
                }
                if (start.list$k2 < 2) {
                    start.list$k2 <- 2.5;
                }
                ## print(start.list);
                ft <- NULL; n.try <- 1;
                while (is.null(ft) && n.try <= kNumTries) {
                    ## print(paste0(">> Fit try ", n.try, " of ", kNumTries, " with start.list = "));
                    ## print(start.list);
                    try(expr = {
                            ft <- suppressWarnings(minpack.lm::nlsLM(
                                y ~ b + A1 * pgamma(q = x - t0, shape = k1, scale = theta) +
                                    A2 * pgamma(q = x - t0, shape = k2, scale = theta) +
                                        (A1 * k.a2m / gamma(k1)) * (
                                            gamma(k1) * pgamma(q = x - t0, shape = k1, scale = theta) * (x - t0) -
                                                gamma(k1 + 1) * theta *
                                                    pgamma(q = x - t0, shape = k1 + 1, scale = theta)) +
                                            (A2 * k.a2m / gamma(k2)) * (
                                                gamma(k2) * pgamma(q = x - t0, shape = k2, scale = theta) * (x - t0) -
                                                    gamma(k2 + 1) * theta *
                                                        pgamma(q = x - t0, shape = k2 + 1, scale = theta)),
                                data = data, start = start.list, trace = F,
                                ## lower = c(b.min, A.min, k.min, theta.min),
                                ## upper = c(b.max, A.max, k.max, theta.max),
                                lower = c(  0,   0,   0, 2.1, 2.1,   0, 1e-5,   0),
                                upper = c(Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf),
                                algorithm = "LM",
                                control = minpack.lm::nls.lm.control(
                                    ftol = sqrt(.Machine$double.eps),
                                    ptol = sqrt(.Machine$double.eps),
                                    gtol = 0, factor = 50,  ## between [0.1, 100]
                                    maxiter = 200, nprint = -1
                                    )
                                ))
                        }, silent = silent);
                    if (!is.null(ft)) {
                        if (!silent)
                            print(">> Fit not NULL, checking dgn = ");
                        dgn <- conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4]);  ## print(dgn);
                        if (dgn[1] <= "5" || summary(ft)$sigma >= kSigmaLMRatio * num.eval$sigma.lm) {
                            if (!silent)
                                print(">> dgn[1] <= 5 OR sigma >= kSigmaLMRatio * sigma.lm, setting ft back to NULL");
                            ft <- NULL;
                        }
                    }
                    n.try <- n.try + 1;
                    start.list <- list(
                        b = runif(1, 0.9, 1.1) * start.list$b,
                        A1 = runif(1, 0.5, 1.5) * start.list$A1,
                        A2 = runif(1, 0.5, 1.5) * start.list$A2,
                        k1 = runif(1, 3, 10), k2 = runif(1, 3, 10),
                        theta = runif(1, 3, 10),
                        k.a2m = runif(1, 0.5, 1.5) * start.list$k.a2m,
                        t0 = runif(1, 0.9, 1.1) * start.list$t0
                        );
                }  ## End of while()
                if (is.null(ft)) {
                    print(">> fit_T0GammaInt2 resulted in NULL!");
                    return(NULL);
                } else {
                    ## print(paste0(">> T0GammaInt2 started at n.try = ", n.try,
                    ##              " with start.list = "));
                    ## print(unlist(start.list));
            fit$T0GammaInt2 <<- list(
                cff = coef(ft), smry = get_compact_summary(ft),  ##summary(ft),
                diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
                );
            if (!silent)
                print(fit[names(fit) != "LM"]);
            return(fit$T0GammaInt2);
        }  ## End of if is.null(fit)
            }  ## End of if exists()
        }), options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$fitT0GammaInt2
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_T0GammaInt2",
    value = compiler::cmpfun(
        f = function() {
            if (!is.null(fit$T0GammaInt2)) {  ## exists(x = "T0GammaInt2", where = fit)
                b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
                A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
                k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
                k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                return(b + A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
                           A2 * pgamma(q = data$x - t0, shape = k2, scale = theta) +
                               (A1 * k.a2m / gamma(k1)) * (
                                   gamma(k1) *
                                       pgamma(q = data$x - t0, shape = k1, scale = theta) * (data$x - t0) -
                                           gamma(k1 + 1) * theta *
                                               pgamma(q = data$x - t0, shape = k1 + 1, scale = theta)) +
                                   (A2 * k.a2m / gamma(k2)) * (
                                       gamma(k2) *
                                           pgamma(q = data$x - t0, shape = k2, scale = theta) * (data$x - t0) -
                                               gamma(k2 + 1) * theta *
                                                   pgamma(q = data$x - t0, shape = k2 + 1, scale = theta))
                       );
            } else {
                print(">> fit$T0GammaInt2 does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE)  ## End of TG$get_T0GammaInt2
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_T0GammaInt2",
    value = compiler::cmpfun(
        f =  function(cal.CF = 1) {
            ## print(">> Call to TG.parms_T0GammaInt2");
            ## if (exists(x = "T0GammaInt2", where = fit)) {  ## envir = environment()
            if (!is.null(fit$T0GammaInt2)) {
                ## print(">> T0GammaInt2 fit not null");
                b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
                A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
                k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
                k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
                ## print(">> Parameters extracted");
                peak1 <- get_peak(A1, k1, theta); peak2 <- get_peak(A2, k2, theta);
                t.peak1 <- get_tpeak(k1, theta, t0); t.peak2 <- get_peak(k2, theta, t0);

                ## print(paste0(">> t.peak1 = ", t.peak1, ", t.peak2 = ", t.peak2));
                ## normal scenario - both peaks happen within the time of the
                ## signal
                if (A1 <= num.smry$ampl && A2 <= num.smry$ampl) {
                    ## normal scenario - both peaks have lower ETP's than signal
                    ## amplitude
                    ETP <- A1 + A2;
                    if (peak1 > peak2) {
                        t.peak <- t.peak1;
                        peak <- peak1 + get_thrombin_contribution("T0GammaInt2", number = 2,
                                                                  time = t.peak);
                        t.vel.peak <- get_vel_tpeak(k1, theta, t0);
                        vel.index <- get_vel_peak(A1, k1, theta) +
                            get_thrombin_vel_contribution("T0GammaInt2", number = 2,
                                                          time = t.vel.peak);
                    } else {
                        t.peak <- t.peak2;
                        peak <- peak2 + get_thrombin_contribution("T0GammaInt2", number = 1,
                                                                  time = t.peak);
                        t.vel.peak <- get_vel_tpeak(k2, theta, t0);
                        vel.index <- get_vel_peak(A2, k2, theta) +
                            get_thrombin_vel_contribution("T0GammaInt2", number = 1,
                                                          time = t.vel.peak);
                    }
                } else {
                    ## one of the peaks (the abnormally high one) is outside the
                    ## signal time / has abnormal amplitude
                    if (peak1 < peak2) {
                        ## peak1 is the normal peak
                        ETP <- A1;  ## print(ETP);
                        t.peak <- t.peak1;  ## print(t.peak);
                        peak <- peak1;
                        ## t.vel.peak <- get_vel_tpeak(k1, theta, t0);
                        vel.index <- get_vel_peak(A1, k1, theta);
                    } else {
                        ## peak2 is the normal peak
                        ETP <- A2;  ## print(ETP);
                        t.peak <- t.peak2;  ## print(t.peak);
                        peak <- peak2;
                        ## t.vel.peak <- get_vel_tpeak(k2, theta, t0);
                        vel.index <- get_vel_peak(A2, k2, theta);
                    }
                }
                ## print(ETP);
                if (cal.CF != 1) {
                    CF <- cal.CF;
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            CF * ETP,
                            CF * peak,
                            t.peak,
                            CF * vel.index,
                            CF * k.a2m * ETP
                            ),
                        Units = kUnits));
                } else {
                    ## print(paste0("vel.index = ", vel.index));
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            ETP,
                            peak,
                            t.peak,
                            vel.index,
                            k.a2m * ETP
                            ),
                        Units = kAUnits));
                }
            } else {
                print(">> fit$T0GammaInt2 does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$parms_T0GammaInt2
################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.fit_T0GammaInt2 <- function(silent = TRUE) {
##     if (!silent)
##         print(">> fit_T0GammaInt2 called!");
##     if (exists(x = "T0GammaInt2", where = fit)) {
##         warning(">> No fitting: T0GammaInt2 fit already exists!");
##         return(fit$T0GammaInt2);
##     } else {
##         if (!exists(x = "T0GammaInt", where = fit)) {
##             fit_T0GammaInt(silent = TRUE);
##         }
##         ## print(fit$T0GammaInt$smry);
##         start.list <- list(
##             b = fit$T0GammaInt$cff[["b"]],
##             A1 = 0.5 * fit$T0GammaInt$cff[["A"]],
##             A2 = 0.5 * fit$T0GammaInt$cff[["A"]],
##             k1 = fit$T0GammaInt$cff[["k"]], k2 = fit$T0GammaInt$cff[["k"]],
##             theta = fit$T0GammaInt$cff[["theta"]],
##             k.a2m = fit$T0GammaInt$cff[["k.a2m"]],
##             t0 = fit$T0GammaInt$cff[["t0"]]
##             );
##         if (start.list$k1 < 2) {
##             start.list$k1 <- 2.5;
##         }
##         if (start.list$k2 < 2) {
##             start.list$k2 <- 2.5;
##         }
##         ## print(start.list);
##         ft <- NULL; n.try <- 1;
##         while (is.null(ft) && n.try <= kNumTries) {
##             ## print(paste0(">> Fit try ", n.try, " of ", kNumTries, " with start.list = "));
##             ## print(start.list);
##             try(expr = {
##                 ft <- suppressWarnings(minpack.lm::nlsLM(
##                     y ~ b + A1 * pgamma(q = x - t0, shape = k1, scale = theta) +
##                         A2 * pgamma(q = x - t0, shape = k2, scale = theta) +
##                         (A1 * k.a2m / gamma(k1)) * (
##                             gamma(k1) * pgamma(q = x - t0, shape = k1, scale = theta) * (x - t0) -
##                                 gamma(k1 + 1) * theta *
##                                     pgamma(q = x - t0, shape = k1 + 1, scale = theta)) +
##                             (A2 * k.a2m / gamma(k2)) * (
##                                 gamma(k2) * pgamma(q = x - t0, shape = k2, scale = theta) * (x - t0) -
##                                     gamma(k2 + 1) * theta *
##                                         pgamma(q = x - t0, shape = k2 + 1, scale = theta)),
##                     data = data, start = start.list, trace = F,
##                     ## lower = c(b.min, A.min, k.min, theta.min),
##                     ## upper = c(b.max, A.max, k.max, theta.max),
##                     lower = c(  0,   0,   0, 2.1, 2.1,   0, 1e-5,   0),
##                     upper = c(Inf, Inf, Inf, Inf, Inf, Inf,  Inf, Inf),
##                     algorithm = "LM",
##                     control = nls.lm.control(
##                         ftol = sqrt(.Machine$double.eps),
##                         ptol = sqrt(.Machine$double.eps),
##                         gtol = 0, factor = 50,  ## between [0.1, 100]
##                         maxiter = 200, nprint = -1
##                     )
##                 ))
##             }, silent = silent);
##             if (!is.null(ft)) {
##                 if (!silent)
##                     print(">> Fit not NULL, checking dgn = ");
##                 dgn <- conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4]);  ## print(dgn);
##                 if (dgn[1] <= "5") {
##                     if (!silent)
##                         print(">> dgn[1] <= 5, setting ft back to NULL");
##                     ft <- NULL;
##                 }
##             }
##             n.try <- n.try + 1;
##             start.list <- list(
##                 b = runif(1, 0.9, 1.1) * start.list$b,
##                 A1 = runif(1, 0.5, 1.5) * start.list$A1,
##                 A2 = runif(1, 0.5, 1.5) * start.list$A2,
##                 k1 = runif(1, 3, 10), k2 = runif(1, 3, 10),
##                 theta = runif(1, 3, 10),
##                 k.a2m = runif(1, 0.5, 1.5) * start.list$k.a2m,
##                 t0 = runif(1, 0.9, 1.1) * start.list$t0
##                 );
##         }  ## End of while()
##         if (is.null(ft)) {
##             warning(">> fit_T0GammaInt2 resulted in NULL!");
##             return(NULL);
##         } else {
##             ## print(paste0(">> T0GammaInt2 started at n.try = ", n.try,
##             ##              " with start.list = "));
##             ## print(unlist(start.list));
##             fit$T0GammaInt2 <<- list(
##                 cff = coef(ft), smry = summary(ft),
##                 diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
##                 );
##             if (!silent)
##                 print(fit[names(fit) != "LM"]);
##             return(fit$T0GammaInt2);
##         }  ## End of if is.null(fit)
##     }  ## End of if exists()
## }  ## End of TG_fitT0GammaInt2
## ################################################################################

## ################################################################################
## TG.get_T0GammaInt2 <- function() {
##     if (exists(x = "T0GammaInt2", where = fit)) {
##         b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
##         A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
##         k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
##         k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];
##         return(b + A1 * pgamma(q = data$x - t0, shape = k1, scale = theta) +
##                    A2 * pgamma(q = data$x - t0, shape = k2, scale = theta) +
##                    (A1 * k.a2m / gamma(k1)) * (
##                        gamma(k1) *
##                            pgamma(q = data$x - t0, shape = k1, scale = theta) * (data$x - t0) -
##                                gamma(k1 + 1) * theta *
##                                    pgamma(q = data$x - t0, shape = k1 + 1, scale = theta)) +
##                        (A2 * k.a2m / gamma(k2)) * (
##                            gamma(k2) *
##                                pgamma(q = data$x - t0, shape = k2, scale = theta) * (data$x - t0) -
##                                    gamma(k2 + 1) * theta *
##                                        pgamma(q = data$x - t0, shape = k2 + 1, scale = theta))
##                );
##     } else {
##         warning(">> fit$T0GammaInt2 does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of TG_get_T0GammaInt2
## ################################################################################

## ################################################################################
## TG.parms_T0GammaInt2 <- function(cal.CF = 1) {
##     ## print(">> Call to TG.parms_T0GammaInt2");
##     if (exists(x = "T0GammaInt2", where = fit)) {
##         b <- fit$T0GammaInt2$cff[["b"]]; A1 <- fit$T0GammaInt2$cff[["A1"]];
##         A2 <- fit$T0GammaInt2$cff[["A2"]]; k1 <- fit$T0GammaInt2$cff[["k1"]];
##         k2 <- fit$T0GammaInt2$cff[["k2"]]; theta <- fit$T0GammaInt2$cff[["theta"]];
##         k.a2m <- fit$T0GammaInt2$cff[["k.a2m"]]; t0 <- fit$T0GammaInt2$cff[["t0"]];

##         peak1 <- get_peak(A1, k1, theta); peak2 <- get_peak(A2, k2, theta);
##         if (peak1 > peak2) {
##             t.peak <- get_tpeak(k = k1, theta = theta, t0 = t0);  ## t0 + theta * (k1 - 1);
##             peak <- peak1 + get_thrombin_contribution("T0GammaInt2", number = 2,
##                                                       time = t.peak);
##             t.vel.peak <- get_vel_tpeak(k = k1, theta = theta, t0 = t0);  ## t0 + theta * (k1 - 1 - sqrt(k1 - 1));
##             vel.index <- get_vel_peak(A1, k1, theta) +
##                 get_thrombin_vel_contribution("T0GammaInt2", number = 2,
##                                               time = t.vel.peak);
##         } else {
##             t.peak <- get_tpeak(k = k2, theta = theta, t0 = t0);  ## t0 + theta * (k2 - 1);
##             peak <- peak2 + get_thrombin_contribution("T0GammaInt2", number = 1,
##                                                       time = t.peak);
##             t.vel.peak <- get_vel_tpeak(k = k2, theta = theta, t0 = t0);  ## t0 + theta * (k2 - 1 - sqrt(k2 - 1));
##             vel.index <- get_vel_peak(A2, k2, theta) +
##                 get_thrombin_vel_contribution("T0GammaInt2", number = 1,
##                                               time = t.vel.peak);
##         }

##         if (cal.CF != 1) {
##             CF <- cal.CF;
##             return(parms <<- data.frame(
##                 Parameter = kParameterNames,
##                 Value = c(
##                     t0,
##                     CF * (A1 + A2),
##                     CF * peak,
##                     t.peak,
##                     CF * vel.index,
##                     CF * k.a2m * (A1 + A2)
##                     ),
##                 Units = kUnits));
##         } else {
##             ## print(paste0("vel.index = ", vel.index));
##             return(parms <<- data.frame(
##                 Parameter = kParameterNames,
##                 Value = c(
##                     t0,
##                     A1 + A2,
##                     peak,
##                     t.peak,
##                     vel.index,
##                     k.a2m * (A1 + A2)
##                     ),
##                 Units = kAUnits));
##         }
##     } else {
##         warning(">> fit$T0GammaInt2 does not exist!");
##         return(NULL);
##     }
## }  ## End of TG.parms_T0GammaInt2
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

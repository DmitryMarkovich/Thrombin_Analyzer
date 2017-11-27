################################################################################
TG$set(
    which = "public", name = "fit_LateExpT0GammaInt",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (!is.null(fit$LateExpT0GammaInt)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                print(">> No fitting: LateExpT0GammaInt fit already exists!");
                return(fit$LateExpT0GammaInt);
            } else {
                if (is.null(fit$T0GammaInt)) {  ## !exists(x = "LateExpGammaInt", where = fit)
                    fit_T0GammaInt(silent = TRUE);
                }
                if (!is.null(fit$T0GammaInt)) {
                    start.list <- list(
                        b = fit$T0GammaInt$cff[["b"]],
                        p1 = fit$T0GammaInt$cff[["A"]],
                        A = runif(1, 0, 2),
                        k = fit$T0GammaInt$cff[["k"]],
                        theta = fit$T0GammaInt$cff[["theta"]],
                        k.a2m = fit$T0GammaInt$cff[["k.a2m"]],
                        t0 = fit$T0GammaInt$cff[["t0"]]);
                    target.sigma <- fit$T0GammaInt$smry$sigma;
                } else {
                    start.list <- list(
                        b = fit$T0GammaInt$cff[["b"]],
                        p1 = fit$T0GammaInt$cff[["A"]],
                        A = runif(1, 0, 2),
                        k = fit$T0GammaInt$cff[["k"]],
                        theta = fit$T0GammaInt$cff[["theta"]],
                        k.a2m = fit$T0GammaInt$cff[["k.a2m"]],
                        t0 = fit$T0GammaInt$cff[["t0"]]);
                    target.sigma <- kSigmaLMRatio * num.eval$sigma.lm;
                }
                ft <- NULL; n.try <- 1;
                while (is.null(ft) && n.try <= kNumTries) {
                    try(expr = {
                            ft <- suppressWarnings(minpack.lm::nlsLM(
                                y ~ b + p1 * (1 - exp(-A * (
                                    pgamma(q = x - t0, shape = k, scale = theta) +
                                        (k.a2m / gamma(k)) * (
                                            gamma(k) * pgamma(q = x - t0, shape = k, scale = theta) * (x - t0) -
                                                gamma(k + 1) * theta *
                                                    pgamma(q = x - t0, shape = k + 1, scale = theta))
                                    ))),
                                data = data, start = start.list, trace = F,
                                ## lower = c(b.min, A.min, k.min, theta.min),
                                ## upper = c(b.max, A.max, k.max, theta.max),
                                lower = c(  0,   0,   0, 1.1,   0,   0,   0),
                                upper = c(Inf, Inf, Inf, Inf, Inf, Inf, Inf),
                                algorithm = "LM",
                                control = minpack.lm::nls.lm.control(
                                    ftol = sqrt(.Machine$double.eps),
                                    ptol = sqrt(.Machine$double.eps),
                                    gtol = 0, factor = 100,  ## between [0.1, 100]
                                    maxiter = 200, nprint = -1
                                    )
                                ))
                        }, silent = TRUE);
                    if (!is.null(ft)) {
                        if (!silent)
                            print(">> Fit not NULL, checking dgn = ");
                        dgn <- conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4]);  ## print(dgn);
                        if (dgn[1] <= "4" ||
                            summary(ft)$sigma >= kSigmaLMRatio * num.eval$sigma.lm ||
                            summary(ft)$sigma >= target.sigma) {
                            if (!silent)
                                print(">> dgn[1] <= 4 OR sigma >= kSigmaLMRatio * sigma.lm OR sigma >= target.sigma, setting ft back to NULL");
                            ft <- NULL;
                        }
                    }
                    n.try <- n.try + 1;
                    start.list <- list(b = data$y[1],
                                       p1 = runif(1, 0, 1) * num.smry$ampl,
                                       A = runif(1, 0, 2), k = runif(1, 2, 10),
                                       theta = runif(1, 1, 10),
                                       k.a2m = runif(1, 0, 1) * 0.05,
                                       t0 = 0);
                }  ## End of while
                if (is.null(ft)) {
                    print(">> fit_LateExpT0GammaInt resulted in NULL!");
                    return(NULL);
                } else {
                    fit$LateExpT0GammaInt <<- list(
                        cff = coef(ft), smry = get_compact_summary(ft),
                        diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
                        );
                    if (!silent)
                        print(fit[names(fit) != "LM"]);
                    return(fit$LateExpT0GammaInt);
                }  ## End of if is.null(fit)
            }  ## End of if exists()
        }, options = kCmpFunOptions),
    overwrite = FALSE)  ## End of TG$fit_LateExpT0GammaInt
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_LateExpT0GammaInt",
    value = compiler::cmpfun(
        f = function() {
            if (!is.null(fit$LateExpT0GammaInt)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                b <- fit$LateExpT0GammaInt$cff[["b"]]; A <- fit$LateExpT0GammaInt$cff[["A"]];
                k <- fit$LateExpT0GammaInt$cff[["k"]]; theta <- fit$LateExpT0GammaInt$cff[["theta"]];
                k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]]; p1 <- fit$LateExpT0GammaInt$cff[["p1"]]
                t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                return(b + p1 * (1 - exp(-A * (
                    pgamma(q = data$x - t0, shape = k, scale = theta) +
                        (k.a2m / gamma(k)) * (
                            gamma(k) *
                        pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                            gamma(k + 1) * theta *
                                pgamma(q = data$x - t0, shape = k + 1, scale = theta))   
                    ))));
            } else {
                print(">> fit$LateExpT0GammaInt does not exist!");
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_LateExpT0GammaInt
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_LateExpT0GammaInt",
    value = compiler::cmpfun(
        f = function(cal.CF) {
            ## print(">> Call to TG.parms_LateExpT0GammaInt");
            if (!is.null(fit$LateExpT0GammaInt)) {  ## exists(x = "LateExpT0GammaInt", where = fit)
                ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
                p1 <- fit$LateExpT0GammaInt$cff[["p1"]];
                A <- fit$LateExpT0GammaInt$cff[["A"]];
                k <- fit$LateExpT0GammaInt$cff[["k"]];
                theta <- fit$LateExpT0GammaInt$cff[["theta"]];
                k.a2m <- fit$LateExpT0GammaInt$cff[["k.a2m"]];
                t0 <- fit$LateExpT0GammaInt$cff[["t0"]];
                tpeak <- get_tpeak(k, theta, t0);
                if (k > 2) {
                    v <- p1 * get_vel_peak(A, k, theta);
                } else {
                    v <- num.eval$parms$Value[num.eval$parms$Parameter == "VelIndex"];
                }
                if (cal.CF != 1) {
                    CF <- cal.CF;
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            CF * p1 * A,
                            CF * p1 * get_peak(A, k, theta),
                            ## t0 + theta * (k - 1),
                            tpeak,
                            CF * v,
                            CF * p1 * k.a2m * A
                            ),
                        ## StdErr = rep(NA, 5),
                        Units = kUnits));
                } else {
                    return(parms <<- data.frame(
                        Parameter = kParameterNames,
                        Value = c(
                            t0,
                            p1 * A,
                            p1 * get_peak(A, k, theta),
                            ## t0 + theta * (k - 1),
                            tpeak,
                            v,
                            p1 * k.a2m * A
                            ),
                        Units = kAUnits));
                }
            } else {
                print(">> fit$LateExpT0GammaInt does not exist!");
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG.parms_LateExpT0GammaInt
################################################################################

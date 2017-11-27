################################################################################
TG$set(
    which = "public", name = "fit_T0GammaInt",
    value = compiler::cmpfun(
        f = function(silent = TRUE) {
            if (!silent)
                print(">> fit_T0GammaInt called!");
            if (exists(x = "T0GammaInt", where = fit, envir = self)) {  ##  !is.null(fit$T0GammaInt)
                print(">> No fitting: T0GammaInt fit already exists!");
                return(fit$T0GammaInt);
            } else {
                ft <- lm(y ~ x, data = data, subset = x >= num.smry$t.lin); #
                A <- coef(ft)[[1]] + coef(ft)[[2]] * num.smry$t.lin;
                k.a2m <- coef(ft)[[2]] / A; k <- 3; theta <- num.smry$t.peak / (k - 1);
                num.eval$sigma.lm <<- summary(ft)$sigma;
                ## print(paste0(">> num.eval$sigma.lm = ", num.eval$sigma.lm));
                start.list <- list(b = data$y[1], A = A, k = k,
                                   theta = theta, k.a2m = k.a2m, t0 = 0);
                ## print(start.list);

                ft <- NULL; n.try <- 1;
                while (is.null(ft) && n.try <= kNumTries) {
                    try(expr = {
                            ft <- suppressWarnings(minpack.lm::nlsLM(
                                y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta) +
                                    (A * k.a2m / gamma(k)) * (
                                        gamma(k) * pgamma(q = x - t0, shape = k, scale = theta) * (x - t0) -
                                            gamma(k + 1) * theta *
                                                pgamma(q = x - t0, shape = k + 1, scale = theta)),
                                data = data, start = start.list, trace = F,
                                ## lower = c(b.min, A.min, k.min, theta.min),
                                ## upper = c(b.max, A.max, k.max, theta.max),
                                lower = c(  0,   0, 1.25,   0, 1e-5, -Inf),
                                upper = c(Inf, Inf,  Inf, Inf,  Inf,  Inf),
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
                        if (dgn[1] <= "4" || summary(ft)$sigma >= kSigmaLMRatio * num.eval$sigma.lm) {
                            if (!silent)
                                print(">> dgn[1] <= 4 OR sigma >= kSigmaLMRatio * sigma.lm, setting ft back to NULL");
                            ft <- NULL;
                        }
                    }
                    n.try <- n.try + 1;
                    start.list <- list(b = data[[2]][1], A = runif(1) * num.smry$ampl,
                                       k = runif(1, 1, 10), theta = runif(1, 1, 30),
                                       k.a2m = runif(1) * 1e-3, t0 = 0);
                }  ## End of while()
                if (is.null(ft)) {
                    print(paste0(">> fit_T0GammaInt resulted in NULL after ", n.try, " tries!"));
                    return(NULL);
                } else {
                    fit$T0GammaInt <<- list(
                        cff = coef(ft), smry = get_compact_summary(ft),  ## summary(ft),
                        diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
                    );
                    if (kFitToDrv) {
                        ## print(">> Start fitting to the derivative");
                        ft.drv <- NULL; n.try <- 1; start.list <- as.list(coef(ft)[-1]); ##print(start.list);
                        str(data.frame(x = data$x, y = num.smry$drv1));
                        while (is.null(ft.drv) && n.try <= kNumTries) {
                            try(expr = {
                                ft.drv <- (minpack.lm::nlsLM( ##suppressWarnings
                                    y ~ A * dgamma(x = x - t0, shape = k, scale = theta) +
                                        A * k.a2m * (
                                            pgamma(q = x - t0, shape = k, scale = theta)),
                                    data = data.frame(x = data$x, y = num.smry$drv1),
                                    start = start.list, trace = F,
                                    ## lower = c(b.min, A.min, k.min, theta.min),
                                    ## upper = c(b.max, A.max, k.max, theta.max),
                                    lower = c(  0, 1.15,   0, 1e-5, -Inf),
                                    upper = c(Inf,  Inf, Inf,  Inf,  Inf),
                                    algorithm = "LM",
                                    control = minpack.lm::nls.lm.control(
                                        ftol = sqrt(.Machine$double.eps),
                                        ptol = sqrt(.Machine$double.eps),
                                        gtol = 0, factor = 100,  ## between [0.1, 100]
                                        maxiter = 200, nprint = -1
                                    )
                                ))
                            }, silent = FALSE);
                            if (!is.null(ft.drv)) {
                                ## if (!silent)
                                print(">> Fit to drv not NULL, checking dgn = ");
                                dgn <- conv_pvals_to_signif_codes(summary(ft.drv)$coefficients[, 4]);  ## print(dgn);
                                if (dgn[1] <= "3") {  ## || summary(ft.drv)$sigma >= kSigmaLMRatio * num.eval$sigma.lm
                                    ## if (!silent)
                                    print(">> dgn[3] <= 4 OR sigma >= kSigmaLMRatio * sigma.lm, setting ft back to NULL");
                                    ft.drv <- NULL;
                                }
                            }
                            n.try <- n.try + 1;
                            start.list <- lapply(start.list, FUN = function(x) {
                                return(x * runif(1, 0.5, 1.5));
                            });
                            ## list(A = runif(1) * num.smry$ampl,
                                ##                k = runif(1, 1, 10), theta = runif(1, 1, 30),
                                ##                k.a2m = runif(1) * 1e-3, t0 = 0);
                        }  ## End of while
                        if (!is.null(ft.drv)) {
                            print(summary(ft.drv));
                        }
                        ## print(">> End of fitting to drv");
                        fit$T0GammaInt <<- list(
                            cff = c(coef(ft)[1], coef(ft.drv)), smry = get_compact_summary(ft.drv),
                            diagn = conv_pvals_to_signif_codes(summary(ft.drv)$coefficients[, 4])
                        );
                    }  ## End of if (kFitToDrv)
                    if (!silent)
                        print(fit[names(fit) != "LM"]);
                    return(fit$T0GammaInt);
                }  ## End of if is.null(ft)
            }  ## End of if exists()
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$fit_T0GammaInt
################################################################################

################################################################################
TG$set(
    which = "public", name = "get_T0GammaInt",
    value = compiler::cmpfun(
        f = function() {
            if (!is.null(fit$T0GammaInt)) {  ## exists(x = "T0GammaInt", where = fit)
                b <- fit$T0GammaInt$cff[["b"]]; A <- fit$T0GammaInt$cff[["A"]];
                k <- fit$T0GammaInt$cff[["k"]]; theta <- fit$T0GammaInt$cff[["theta"]];
                k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
                return(b + A * pgamma(q = data$x - t0, shape = k, scale = theta) +
                           (A * k.a2m / gamma(k)) * (
                               gamma(k) *
                                   pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
                                       gamma(k + 1) * theta *
                                           pgamma(q = data$x - t0, shape = k + 1, scale = theta))
                       );
            } else {
                print(">> fit$T0GammaInt does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$get_T0GammaInt
################################################################################

################################################################################
TG$set(
    which = "public", name = "parms_T0GammaInt",
    value = compiler::cmpfun(
        f = function(cal.CF = 1) {
            ## print(">> Call to TG.parms_T0GammaInt");
            if (!is.null(fit$T0GammaInt)) {
            A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
            theta <- fit$T0GammaInt$cff[["theta"]];
            t0 <- fit$T0GammaInt$cff[["t0"]]; k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; 
            if (k > 2) {
                v <- get_vel_peak(A, k, theta);
            } else {
                v <- num.eval$parms$Value[num.eval$parms$Parameter == "VelIndex"];
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
                        CF * k.a2m * A
                        ),
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
                        k.a2m * A
                        ),
                    Units = kAUnits));
            }
        } else {
            print(">> fit$T0GammaInt does not exist!");
            return(NULL);
        }
    }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of TG$parms_T0GammaInt
################################################################################

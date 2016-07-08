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
                ft <- lm(y ~ x, data = data, subset = x >= num.smry$t.lin);
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
                                lower = c(  0,   0, 1.25,   0, 1e-5,   0),
                                upper = c(Inf, Inf,  Inf, Inf,  Inf, Inf),
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
                    if (!silent)
                        print(fit[names(fit) != "LM"]);
                    return(fit$T0GammaInt);
                }  ## End of if is.null(fit)
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
            ## if (exists(x = "T0GammaInt", where = fit)) {  ## , envir = environment()
            if (!is.null(fit$T0GammaInt)) {
            A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
            theta <- fit$T0GammaInt$cff[["theta"]];
            t0 <- fit$T0GammaInt$cff[["t0"]]; k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; 
            if (k > 2) {
                v <- get_vel_peak(A, k, theta);  ## A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
                    ## exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
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

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## TG.fit_T0GammaInt <- function(silent = TRUE) {
##     if (!silent)
##         print(">> fit_T0GammaInt called!");
##     if (exists(x = "T0GammaInt", where = fit)) {
##         warning(">> No fitting: T0GammaInt fit already exists!");
##         return(fit$T0GammaInt);
##     } else {
##         ## if (exists(x = "GammaInt", where = fit)) {
##         ##     start.list <- list(
##         ##         b = fit$GammaInt$cff[["b"]], A = fit$GammaInt$cff[["A"]],
##         ##         k = fit$GammaInt$cff[["k"]], theta = fit$GammaInt$cff[["theta"]],
##         ##         k.a2m = fit$GammaInt$cff[["k.a2m"]], t0 = 0);
##         ## } else {
##         ##     fit_GammaInt(silent = TRUE);
##         ##     start.list <- list(
##         ##         b = fit$GammaInt$cff[["b"]], A = fit$GammaInt$cff[["A"]],
##         ##         k = fit$GammaInt$cff[["k"]], theta = fit$GammaInt$cff[["theta"]],
##         ##         k.a2m = fit$GammaInt$cff[["k.a2m"]], t0 = 0);
##         ## }
##         ft <- lm(y ~ x, data = data, subset = x >= num.smry$t.lin);
##         A <- coef(ft)[[1]] + coef(ft)[[2]] * num.smry$t.lin;
##         k.a2m <- coef(ft)[[2]] / A; k <- 3; theta <- num.smry$t.peak / (k - 1);
##         start.list <- list(b = data$y[1], A = A, k = k,
##                                        theta = theta, k.a2m = k.a2m, t0 = 0);
##         ## print(start.list);

##         ft <- NULL; n.try <- 1;
##         while (is.null(ft) && n.try <= kNumTries) {
##             try(expr = {
##                 ft <- suppressWarnings(nlsLM(
##                     y ~ b + A * pgamma(q = x - t0, shape = k, scale = theta) +
##                         (A * k.a2m / gamma(k)) * (
##                             gamma(k) * pgamma(q = x - t0, shape = k, scale = theta) * (x - t0) -
##                                 gamma(k + 1) * theta *
##                                     pgamma(q = x - t0, shape = k + 1, scale = theta)),
##                     data = data, start = start.list, trace = F,
##                     ## lower = c(b.min, A.min, k.min, theta.min),
##                     ## upper = c(b.max, A.max, k.max, theta.max),
##                     lower = c(  0,   0, 1.25,   0, 1e-5,   0),
##                     upper = c(Inf, Inf,  Inf, Inf,  Inf, Inf),
##                     algorithm = "LM",
##                     control = nls.lm.control(
##                         ftol = sqrt(.Machine$double.eps),
##                         ptol = sqrt(.Machine$double.eps),
##                         gtol = 0, factor = 100,  ## between [0.1, 100]
##                         maxiter = 200, nprint = -1
##                     )
##                 ))
##             }, silent = silent);
##             if (!is.null(ft)) {
##                 if (!silent)
##                     print(">> Fit not NULL, checking dgn = ");
##                 dgn <- conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4]);  ## print(dgn);
##                 if (dgn[1] <= "4") {
##                     if (!silent)
##                         print(">> dgn[1] <= 4, setting ft back to NULL");
##                     ft <- NULL;
##                 }
##             }
##             n.try <- n.try + 1;
##             start.list <- list(b = data[[2]][1], A = runif(1) * num.smry$ampl,
##                                k = runif(1, 1, 10), theta = runif(1, 1, 30),
##                                k.a2m = runif(1) * 1e-3, t0 = 0);
##         }  ## End of while()
##         if (is.null(ft)) {
##             warning(">> fit_T0GammaInt resulted in NULL!");
##             return(NULL);
##         } else {
##             fit$T0GammaInt <<- list(
##                 cff = coef(ft), smry = summary(ft),
##                 diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
##             );
##             if (!silent)
##                 print(fit[names(fit) != "LM"]);
##             return(fit$T0GammaInt);
##         }  ## End of if is.null(fit)
##     }  ## End of if exists()
## }  ## End of TG_fitT0GammaInt
## ################################################################################

## ################################################################################
## TG.get_T0GammaInt <- function() {
##     if (exists(x = "T0GammaInt", where = fit)) {
##         b <- fit$T0GammaInt$cff[["b"]]; A <- fit$T0GammaInt$cff[["A"]];
##         k <- fit$T0GammaInt$cff[["k"]]; theta <- fit$T0GammaInt$cff[["theta"]];
##         k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; t0 <- fit$T0GammaInt$cff[["t0"]];
##         return(b + A * pgamma(q = data$x - t0, shape = k, scale = theta) +
##                    (A * k.a2m / gamma(k)) * (
##                        gamma(k) *
##                            pgamma(q = data$x - t0, shape = k, scale = theta) * (data$x - t0) -
##                                gamma(k + 1) * theta *
##                                    pgamma(q = data$x - t0, shape = k + 1, scale = theta))
##                );
##     } else {
##         warning(">> fit$T0GammaInt does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of TG_get_T0GammaInt
## ################################################################################

## ################################################################################
## TG.parms_T0GammaInt <- function(cal.CF = 1) {
##     ## print(">> Call to TG.parms_T0GammaInt");
##     if (exists(x = "T0GammaInt", where = fit)) {
##         A <- fit$T0GammaInt$cff[["A"]]; k <- fit$T0GammaInt$cff[["k"]];
##         theta <- fit$T0GammaInt$cff[["theta"]];
##         t0 <- fit$T0GammaInt$cff[["t0"]]; k.a2m <- fit$T0GammaInt$cff[["k.a2m"]]; 
##         if (k > 2) {
##             v <- A * sqrt(k - 1) * (k - 1 - sqrt(k - 1)) ^ (k - 2) *
##                 exp(-(k - 1 - sqrt(k - 1))) / (gamma(k) * theta ^ 2);
##         } else {
##             v <- num.eval$parms$Value[num.eval$parms$Parameter == "VelIndex"];
##         }
##         if (cal.CF != 1) {
##             CF <- cal.CF;
##             return(parms <<- data.frame(
##                 Parameter = kParameterNames,
##                 Value = c(
##                     t0,
##                     CF * A,
##                     CF * A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     CF * v,
##                     CF * k.a2m * A
##                     ),
##                 Units = kUnits));
##         } else {
##             return(parms <<- data.frame(
##                 Parameter = kParameterNames,
##                 Value = c(
##                     t0,
##                     A,
##                     A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta),
##                     t0 + theta * (k - 1),
##                     v,
##                     k.a2m * A
##                     ),
##                 Units = kAUnits));
##         }
##     } else {
##         warning(">> fit$T0GammaInt does not exist!");
##         return(NULL);
##     }
## }  ## End of TG.parms_T0GammaInt
## ################################################################################

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

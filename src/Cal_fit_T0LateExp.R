################################################################################
Cal$set(
    which = "public", name = "fit_T0LateExp",
    value = compiler::cmpfun(
        f = function(silent = FALSE) {
            if (exists(x = "T0LateExp", where = fit)) {
                warning(">> No fitting: T0LateExp fit already exists!");
                return(fit$T0LateExp);
            } else {
                print(">> fit_T0LateExp called!");
                if (!exists(x = "LateExp", where = fit)) {
                    fit_LateExp(silent = TRUE);
                }
                start.list <- list(b = fit$LateExp$cff[["b"]], p1 = fit$LateExp$cff[["p1"]],
                                   p3 = fit$LateExp$cff[["p3"]], t0 = 0);
                ft <- NULL; n.try <- 1;
                while (is.null(ft) && n.try <= kNumTries) {
                    try(expr = {
                            ft <- minpack.lm::nlsLM(
                                y ~ b + (x >= t0) * p1 * (1 - exp(-p3 * (x - t0))),
                                data = data, start = start.list, algorithm = "LM",
                                lower = c(  0,   0,   0,   0),
                                upper = c(Inf, Inf, Inf, Inf),
                                control = nls.lm.control(
                                    ftol = sqrt(.Machine$double.eps),
                                    ptol = sqrt(.Machine$double.eps),
                                    gtol = 0, nprint = -1, factor = 100,  ## between [0.1, 100]
                                    maxiter = 200)
                                )
                        }, silent = F);
                    n.try <- n.try + 1;
                    start.list <- list(b = data$y[1], p1 = runif(1, 0, 2) * num.smry$ampl,
                                       p3 = runif(1, 0, 2) * fit$LM$cff[[2]] / num.smry$ampl,
                                       t0 = 0);
                }
                if (is.null(ft)) {
                    warning(">> Cal.fit_T0LateExp resulted in NULL");
                    return(NULL);
                } else {
                    fit$T0LateExp <<- list(
                        cff = coef(ft), smry = summary(ft),
                        diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
                        );
                    if (!silent)
                        print(fit[names(fit) != "LM"]);
                    return(fit$T0LateExp);
                }  ## End of if (is.null(ft))
            }  ## End of if (exists())
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$fit_T0LateExp
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_T0LateExp",
    value = compiler::cmpfun(
        f = function() {
            if (exists(x = "T0LateExp", where = fit)) {
                b <- fit$T0LateExp$cff[["b"]]; p1 <- fit$T0LateExp$cff[["p1"]];
                p3 <- fit$T0LateExp$cff[["p3"]]; t0 <- fit$T0LateExp$cff[["t0"]];
                return(b + (data$x >= t0) * p1 * (1 - exp(-p3 * (data$x - t0))));
            } else {
                warning(">> fit$T0LateExp does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_T0LateExp
################################################################################

################################################################################
Cal$set(
    which = "public", name = "get_init_rate_T0LateExp",
    value = compiler::cmpfun(
        f = function() {
            print(">> Call to Cal.parms_T0LateExp");
            if (exists(x = "T0LateExp", where = fit)) {
                b <- fit$T0LateExp$cff[["b"]]; p1 <- fit$T0LateExp$cff[["p1"]];
                p3 <- fit$T0LateExp$cff[["p3"]]; t0 <- fit$T0LateExp$cff[["t0"]];
                return(b + (data$x >= t0) * (p1 * p3) * (data$x - t0));
            } else {
                warning(">> fit$T0LateExp does not exist!");
                return(rep(0, length(data$x)));
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$get_init_rate_T0LateExp
################################################################################

################################################################################
Cal$set(
    which = "public", name = "parms_T0LateExp",
    value = compiler::cmpfun(
        f = function(e0, s0) {
            if (exists(x = "T0LateExp", where = fit)) {
                ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
                p1 <- fit$T0LateExp$cff[["p1"]]; p3 <- fit$T0LateExp$cff[["p3"]];
                return(parms <<- data.frame(
                    Parameter = kParameterNamesLM,
                    Value = c(e0, s0, e0 / (p1 * p3), p1 * p3),
                    ## StdErr = rep(NA, 3),
                    Units = kParameterUnitsLM
                    ));
            } else {
                warning(">> fit$T0LateExp does not exist!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Cal$parms_T0LateExp
################################################################################

################################################################################
######################################## Legacy RF classes code
################################################################################

## ################################################################################
## Cal.fit_T0LateExp <- function(silent = FALSE) {
##     if (exists(x = "T0LateExp", where = fit)) {
##         warning(">> No fitting: T0LateExp fit already exists!");
##         return(fit$T0LateExp);
##     } else {
##         print(">> fit_T0LateExp called!");
##         if (!exists(x = "LateExp", where = fit)) {
##             fit_LateExp(silent = TRUE);
##         }
##         start.list <- list(b = fit$LateExp$cff[["b"]], p1 = fit$LateExp$cff[["p1"]],
##                            p3 = fit$LateExp$cff[["p3"]], t0 = 0);
##         ft <- NULL; n.try <- 1;
##         while (is.null(ft) && n.try <= kNumTries) {
##             try(expr = {
##                 ft <- minpack.lm::nlsLM(
##                     y ~ b + (x >= t0) * p1 * (1 - exp(-p3 * (x - t0))),
##                     data = data, start = start.list, algorithm = "LM",
##                     lower = c(  0,   0,   0,   0),
##                     upper = c(Inf, Inf, Inf, Inf),
##                     control = nls.lm.control(
##                         ftol = sqrt(.Machine$double.eps),
##                         ptol = sqrt(.Machine$double.eps),
##                         gtol = 0, nprint = -1, factor = 100,  ## between [0.1, 100]
##                         maxiter = 200
##                     )
##                 )
##             }, silent = F);
##             n.try <- n.try + 1;
##             start.list <- list(b = data$y[1], p1 = runif(1, 0, 2) * num.smry$ampl,
##                                p3 = runif(1, 0, 2) * fit$LM$cff[[2]] / num.smry$ampl,
##                                t0 = 0);
##         }
##         if (is.null(ft)) {
##             warning(">> Cal.fit_T0LateExp resulted in NULL");
##             return(NULL);
##         } else {
##             fit$T0LateExp <<- list(
##                 cff = coef(ft), smry = summary(ft),
##                 diagn = conv_pvals_to_signif_codes(summary(ft)$coefficients[, 4])
##             );
##             if (!silent)
##                 print(fit[names(fit) != "LM"]);
##             return(fit$T0LateExp);
##         }  ## End of if (is.null(ft))
##     }  ## End of if (exists())
## }  ## End of Cal.fit_T0LateExp
## ################################################################################

## ################################################################################
## Cal.get_T0LateExp <- function() {
##     if (exists(x = "T0LateExp", where = fit)) {
##         b <- fit$T0LateExp$cff[["b"]]; p1 <- fit$T0LateExp$cff[["p1"]];
##         p3 <- fit$T0LateExp$cff[["p3"]]; t0 <- fit$T0LateExp$cff[["t0"]];
##         return(b + (data$x >= t0) * p1 * (1 - exp(-p3 * (data$x - t0))));
##     } else {
##         warning(">> fit$T0LateExp does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of Cal.get_T0LateExp
## ################################################################################

## ################################################################################
## Cal.get_init_rate_T0LateExp <- function() {
##     print(">> Call to Cal.parms_T0LateExp");
##     if (exists(x = "T0LateExp", where = fit)) {
##         b <- fit$T0LateExp$cff[["b"]]; p1 <- fit$T0LateExp$cff[["p1"]];
##         p3 <- fit$T0LateExp$cff[["p3"]]; t0 <- fit$T0LateExp$cff[["t0"]];
##         return(b + (data$x >= t0) * (p1 * p3) * (data$x - t0));
##     } else {
##         warning(">> fit$T0LateExp does not exist!");
##         return(rep(0, length(data$x)));
##     }
## }  ## End of Cal.get_init_rate_T0LateExp
## ################################################################################

## ################################################################################
## Cal.parms_T0LateExp <- function(e0, s0) {
##     if (exists(x = "T0LateExp", where = fit)) {
##         ## print(e0); print(s0); print(e0 / fit$LM$cff[[2]]);
##         p1 <- fit$T0LateExp$cff[["p1"]]; p3 <- fit$T0LateExp$cff[["p3"]];
##         return(parms <<- data.frame(
##             Parameter = c("e0", "s0", "CF_CAT", "TC_Initial_Slope"),
##             Value = c(e0, s0, e0 / (p1 * p3), p1 * p3),
##             ## StdErr = rep(NA, 3),
##             Units = c("nM", "uM", "nM * min / a.u.", "a.u. / min"))
##                );
##     } else {
##         warning(">> fit$T0LateExp does not exist!");
##         return(NULL);
##     }
## }  ## End of Cal.parms_T0LateExp
## ################################################################################

################################################################################
######################################## End of Legacy RF classes code
################################################################################

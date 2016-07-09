################################################################################
Base <- R6::R6Class(
    classname = "Base", portable = FALSE,  ## enables <<-
    public = list(
        model_exists = compiler::cmpfun(
            f = function(model) {
                return(!is.null(fit[[model]]));  ## exists(x = model, where = fit)
            }, options = kCmpFunOptions),
        is_none_auto_model = compiler::cmpfun(
            f = function() {
                return(!is.null(fit$Auto_model) && fit$Auto_model == "None");
            }, options = kCmpFunOptions),
        get_summary = compiler::cmpfun(
            f = function(model) {
                if (model == "Auto") {
                    if (fit$Auto_model == "None") {
                        return(NULL);
                    } else {
                        return(fit[[fit$Auto_model]]$smry);
                    }
                } else {
                    return(fit[[model]]$smry);
                }
            }, options = kCmpFunOptions),
        print_summary = compiler::cmpfun(
            f = function(smry) {
                if (!is.null(smry)) {
                    return(capture.output(print(smry)));
                } else {
                    warning(">> smry == NULL!");
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        get_compact_summary = compiler::cmpfun(
            f = function(fit) {
                tmp <- summary(fit);
                ## tmp$formula <- NULL;
                tmp$residuals <- NULL;
                tmp$call <- NULL;
                tmp$cov.unscaled <- NULL;
                ## tmp$convInfo <- NULL;
                tmp$control <- NULL;
                tmp$parameters <- NULL;
                return(tmp);
            }, options = kCmpFunOptions),
        ## from Rcpp_functions.cpp
        drv1 = drv1, drv2 = drv2
        )
    );  ## End of Base
################################################################################

################################################################################
Base$set(
    which = "public", name = "load_signal",
    value = compiler::cmpfun(
        f = function(inFile) {
            ## print(">> load_signal called!");
            ## print(sub(pattern = ".*[.]", "", inFile$name));
            switch(sub(pattern = ".*[.]", "", inFile$name),
                   "dat" = {
                       data <<- read.table(file = inFile$datapath, header = TRUE,
                                           col.names = c("x", "y"), sep = " ");
                   },
                   "csv" = {
                       data <<- read.csv(file = inFile$datapath, header = TRUE,
                                         col.names = c("x", "y"), sep = ";");
                   }
                   )
            data <<- data[complete.cases(data), ];
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Base$load_signal
################################################################################

################################################################################
Base$set(
    which = "public", name = "explore_numerically",
    value = compiler::cmpfun(
        f = function(n = 3, silent = TRUE) {
            if (!is.null(data) && length(num.smry) <= 1) {
                ## length(list()) in R6 private field is 1!!!
                if (!silent)
                    print(">> explore_numerically called!");
                dt <- data$x[2] - data$x[1]; N <- length(data$x);
                ampl <- max(data$y, na.rm = TRUE);
                rat <- list(x = NA, y = abs(ampl / data$y[[1]]));
                if (rat$y <= kYNone) {
                    if (!silent)
                        warning(">> Skipping calculation of derivatives, rat$y <= 3!");
                    num.smry <<- list(rat = rat, t.peak = NA, t.lin = NA, ampl = ampl,
                                      cutoff = NA, drv1 = NA, drv2 = NA);
                    return(0L);
                } else {
                    ## drv1 <- rep(NA, N);
                    ## ## drv1[1:(N - 1)] <- (1 / dt) * (data$y[2:N] - data$y[1:(N - 1)]);
                    ## drv1[1:(N - 3)] <- (1 / (4 * dt)) * (
                    ##     -data$y[1:(N - 3)] - data$y[2:(N - 2)] + data$y[3:(N - 1)] +
                    ##         data$y[4:N]);
                    drv1 <- drv1(data$y, dt);
                    ## drv2 <- rep(NA, N);
                    ## drv2[2:(N - 1)] <- (1 / (dt ^ 2)) * (-2 * data$y[2:(N - 1)] +
                    ##                                          data$y[1:(N - 2)] + data$y[3:N]);
                    ## drv2[1:(N - 4)] <- (1 / (4 * dt ^ 2)) * (
                    ##     data$y[1:(N - 4)] -2 * data$y[3:(N - 2)] + data$y[5:N]);
                    drv2 <- drv2(data$y, dt);
                    cutoff <- median(drv1, na.rm = TRUE);
                    t.peak <- data$x[drv1 == max(drv1, na.rm = TRUE)][1];
                    t.lin <- data$x[data$x >= t.peak & (drv1 <= 1.0 * cutoff) == TRUE][1];
                    ## t.lin <- data$x[sum(drv1 >= cutoff, na.rm = TRUE)];
                    rat$x = data$x[N] / t.peak
                    num.smry <<- list(rat = rat, t.peak = t.peak, t.lin = t.lin,
                                      ampl = ampl, cutoff = cutoff, drv1 = drv1, drv2 = drv2);
                    ## print(num.smry);
                    return(0L);
                }
            } else {
                ## print(data);
                ## print(num.smry);
                ## print(length(num.smry));
                warning(">> num.smry not changed: data == NULL or num.smry not empty.");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Base$explore_numerically
################################################################################

################################################################################
Base$set(
    which = "public", name = "conv_pvals_to_signif_codes",
    value = compiler::cmpfun(
        f = function(pvals) {
            ## Use the symnum function to produce the symbols
            return(
                paste(
                    rev(table(
                        symnum(c(0.001, 0.01, 0.05, 0.1, 1, pvals), na = FALSE,
                               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                               symbols = c("***", "**", "*", ".", " "), legend = F)
                        ) - 1
                        ), collapse = ""
                    )
                );
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Base$conv_pvals_to_signif_codes
################################################################################

################################################################################
Base$set(
    which = "public", name = "compare_two_models",
    value = compiler::cmpfun(
        f = function(model1, model2, ft1, ft2, silent = TRUE) {
            if (is.null(ft1)) {
                if (!silent) {
                    print(paste0(">> ", model1, " does not exist!"));
                    print(paste0(">> Returning ", model2,
                                   " without comparison!"));
                }
                fit$Auto <<- TRUE; fit$Auto_model <<- model2;
                return(0L);
            } else if (is.null(ft2)) {
                if (!silent) {
                    print(paste0(">> ", model2, " does not exist!"));
                    print(paste0(">> Returning ", model1,
                                   " without comparison!"));
                }
                fit$Auto <<- TRUE; fit$Auto_model <<- model1;
                return(0L);
            } else {
                if (ft1$smry$sigma <= ft2$smry$sigma) {
                    if (!silent)
                        print(paste0(">> Returning ", model1,
                                     " because of lower sigma!"));
                    fit$Auto <<- TRUE; fit$Auto_model <<- model1;
                    fit[[model2]] <<- NULL;
                    return(0L);
                } else {
                    if (!silent)
                        print(paste0(">> Returning ", model2,
                                     " because of lower sigma!"));
                    fit$Auto <<- TRUE; fit$Auto_model <<- model2;
                    fit[[model1]] <<- NULL;
                    return(0L);
                }
            }  ## End of if()
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Base$compare_two_models
################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## Base.load_signal <- function(inFile) {
##     print(">> load_signal called!");
##     ## print(sub(pattern = ".*[.]", "", inFile$name));
##     switch(sub(pattern = ".*[.]", "", inFile$name),
##            "dat" = {
##                data <<- read.table(file = inFile$datapath, header = TRUE,
##                                    col.names = c("x", "y"), sep = " ");
##            },
##            "csv" = {
##                data <<- read.csv(file = inFile$datapath, header = TRUE,
##                                    col.names = c("x", "y"), sep = ";");
##            }
##            )
##     data <<- data[complete.cases(data), ];
## }  ## End of Base.LoadSignal
## ################################################################################

## ################################################################################
## Base.explore_numerically <- function(n = 3, silent = TRUE) {
##     if (!is.null(data) && length(num.smry) == 0) {
##         if (!silent)
##             print(">> explore_numerically called!");
##         dt <- data$x[2] - data$x[1]; N <- length(data$x);
##         ampl <- max(data$y, na.rm = TRUE);
##         rat <- list(x = NA, y = ampl / data$y[[1]]);
##         if (rat$y <= kYNone) {
##             if (!silent)
##                 warning(">> Skipping calculation of derivatives, rat$y <= 3!");
##             num.smry <<- list(rat = rat, t.peak = NA, t.lin = NA, ampl = ampl,
##                               cutoff = NA, drv1 = NA, drv2 = NA);
##             return(0L);
##         } else {
##             drv1 <- rep(NA, N);
##             ## drv1[1:(N - 1)] <- (1 / dt) * (data$y[2:N] - data$y[1:(N - 1)]);
##             drv1[1:(N - 3)] <- (1 / (4 * dt)) * (
##                 -data$y[1:(N - 3)] -data$y[2:(N - 2)] + data$y[3:(N - 1)] +
##                     data$y[4:N]);
##             drv2 <- rep(NA, N);
##             ## drv2[2:(N - 1)] <- (1 / (dt ^ 2)) * (-2 * data$y[2:(N - 1)] +
##             ##                                          data$y[1:(N - 2)] + data$y[3:N]);
##             drv2[1:(N - 4)] <- (1 / (4 * dt ^ 2)) * (
##                 data$y[1:(N - 4)] -2 * data$y[3:(N - 2)] + data$y[5:N]);
##             cutoff <- median(drv1, na.rm = TRUE);
##             t.peak <- data$x[drv1 == max(drv1, na.rm = TRUE)][1];
##             t.lin <- data$x[data$x >= t.peak & (drv1 <= 1.0 * cutoff) == TRUE][1];
##             ## t.lin <- data$x[sum(drv1 >= cutoff, na.rm = TRUE)];
##             rat$x = data$x[N] / t.peak
##             num.smry <<- list(rat = rat, t.peak = t.peak, t.lin = t.lin,
##                               ampl = ampl, cutoff = cutoff, drv1 = drv1, drv2 = drv2);
##             ## print(num.smry);
##             return(0L);
##         }
##     } else {
##         warning(">> num.smry not changed: data == NULL or num.smry not empty.");
##         return(NULL);
##     }
## }  ## End of Base.ExploreNumerically
## ################################################################################

## ################################################################################
## Base.ConvPValsToSignifCodes <- function(pvals) {
##     ## Use the symnum function to produce the symbols
##     return(
##         paste(
##             rev(table(
##                 symnum(c(0.001, 0.01, 0.05, 0.1, 1, pvals), na = FALSE,
##                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
##                        symbols = c("***", "**", "*", ".", " "), legend = F)
##             ) - 1
##                 ), collapse = ""
##         )
##     );
## }  ## End of Base.ConvPValsToSignifCodes
## ################################################################################

## ################################################################################
## Base <- setRefClass(
##     Class = "Base",
##     methods = list(
##         load_signal = Base.load_signal,
##         explore_numerically = Base.explore_numerically,
##         conv_pvals_to_signif_codes = Base.ConvPValsToSignifCodes
##     )
## );  ## End of Base
## ################################################################################

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

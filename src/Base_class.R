################################################################################
Base.load_signal <- function(inFile) {
    print(">> load_signal called!");
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
}  ## End of Base.LoadSignal
################################################################################

################################################################################
Base.explore_numerically <- function(n = 3, silent = TRUE) {
    if (!is.null(data) && length(num.smry) == 0) {
        if (!silent)
            print(">> explore_numerically called!");
        dt <- data$x[2] - data$x[1]; N <- length(data$x);
        ampl <- max(data$y, na.rm = TRUE);
        rat <- list(x = NA, y = ampl / data$y[[1]]);
        if (rat$y <= kYNone) {
            if (!silent)
                warning(">> Skipping calculation of derivatives, rat$y <= 3!");
            num.smry <<- list(rat = rat, t.peak = NA, t.lin = NA, ampl = ampl,
                              cutoff = NA, drv1 = NA, drv2 = NA);
            return(0L);
        } else {
            drv1 <- rep(NA, N);
            ## drv1[1:(N - 1)] <- (1 / dt) * (data$y[2:N] - data$y[1:(N - 1)]);
            drv1[1:(N - 3)] <- (1 / (4 * dt)) * (
                -data$y[1:(N - 3)] -data$y[2:(N - 2)] + data$y[3:(N - 1)] +
                    data$y[4:N]);
            drv2 <- rep(NA, N);
            ## drv2[2:(N - 1)] <- (1 / (dt ^ 2)) * (-2 * data$y[2:(N - 1)] +
            ##                                          data$y[1:(N - 2)] + data$y[3:N]);
            drv2[1:(N - 4)] <- (1 / (4 * dt ^ 2)) * (
                data$y[1:(N - 4)] -2 * data$y[3:(N - 2)] + data$y[5:N]);
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
        warning(">> num.smry not changed: data == NULL or num.smry not empty.");
        return(NULL);
    }
}  ## End of Base.ExploreNumerically
################################################################################

################################################################################
Base.ConvPValsToSignifCodes <- function(pvals) {
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
}  ## End of Base.ConvPValsToSignifCodes
################################################################################

################################################################################
Base <- setRefClass(
    Class = "Base",
    methods = list(
        load_signal = Base.load_signal,
        explore_numerically = Base.explore_numerically,
        conv_pvals_to_signif_codes = Base.ConvPValsToSignifCodes
    )
);  ## End of Base
################################################################################

################################################################################
BaseR6 <- R6::R6Class(
    classname = "Base", portable = FALSE,  ## enables <<-
    ## private = list(data = data.frame()),
    public = list()
    );  ## End of BaseR6
################################################################################

################################################################################
BaseR6$set(
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
    overwrite = FALSE);  ## End of BaseR6$conv_pvals_to_signif_codes
################################################################################

################################################################################
BaseR6$set(
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
    overwrite = FALSE);  ## End of BaseR6$load_signal
################################################################################

################################################################################
BaseR6$set(
    which = "public", name = "explore_numerically",
    value = compiler::cmpfun(
        f = function(n = 3, silent = TRUE) {
            if (!is.null(data) && length(num.smry) == 0) {
                if (!silent)
                    print(">> explore_numerically called!");
                dt <- data$x[2] - data$x[1]; N <- length(data$x);
                ampl <- max(data$y, na.rm = TRUE);
                rat <- list(x = NA, y = ampl / data$y[[1]]);
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
                warning(">> num.smry not changed: data == NULL or num.smry not empty.");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of BaseR6$explore_numerically
################################################################################
## print(BaseR6);

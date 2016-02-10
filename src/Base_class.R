################################################################################
Base.load_signal <- function(inFile) {
    print(">> load_signal called!");
    ## print(sub(pattern = ".*[.]", "", inFile$name));
    switch(sub(pattern = ".*[.]", "", inFile$name),
           "dat" = {
               data <<- read.table(file = inFile$datapath, header = TRUE,
                                   col.names = c("x", "y"), sep = " ")
           }
           )
}  ## End of Base.LoadSignal
################################################################################

################################################################################
Base.explore_numerically <- function(n = 3) {
    if (!is.null(data) && length(num.smry) == 0) {
        print(">> explore_numerically called!");
        dt <- data$x[2] - data$x[1]; N <- length(data$x); ampl <- max(data$y);
        drv1 <- rep(NA, N);
        drv1[1:(N - 3)] = (1 / (4 * dt)) * (
            -data$y[1:(N - 3)] -data$y[2:(N - 2)] + data$y[3:(N - 1)] +
                data$y[4:N]);
        cutoff <- median(drv1, na.rm = TRUE);
        t.peak <- data$x[drv1 == max(drv1, na.rm = TRUE)][1];
        t.lin <- data$x[sum(drv1 >= cutoff, na.rm = TRUE)];
        rat <- list(x = data$x[N] / t.peak, y = ampl / min(data$y));
        num.smry <<- list(rat = rat, t.peak = t.peak, t.lin = t.lin,
                          ampl = ampl, cutoff = cutoff, drv1 = drv1);
    } else {
        warning(">> num.smry not changed: data == NULL or num.smry not empty.");
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

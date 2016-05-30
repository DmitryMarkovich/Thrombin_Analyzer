################################################################################
GetSummary <- function(smry, model, full = FALSE) {
    print(smry);
    x <- capture.output(print(smry));  ## print
    ## x <- printed("print(fit$smry)");
    if (full || model == "LM") {
        return(x);
    } else {
        x <- x[x != ""];
        ## return(c(x[((1:length(x))[x ==
        ## "Parameters:"]):((1:length(x))[substr(x, 0, 3) == "---"] - 1)],
        ## x[substr(x, 0, 8) == "Residual"]));
        x <- x[substr(x, 0, 7) != "Signif."];
        y <- c(x[((1:length(x))[x == "Parameters:"]):((1:length(x))[substr(x, 0, 8) == "Residual"])]);
        y[length(y)] <- paste(substr(y[length(y)], 0, nchar(y[length(y)]) - 19), "d.o.f.");
        return(y);
    }
}  ## End of GetSummary()
################################################################################

################################################################################
CalculatePlotLayout <- function(N) {
    if (N != 0) {
        i1 <- round(sqrt(N)); i2 <- ceiling(sqrt(N));
        if (i1 * i2 >= N) {
            ## if (i2 > 10) {
            ##     return(c(ceiling(i1 * i2 / 10), 10));
            ## } else {
            ##     return(c(i1, i2));
            ## }
            return(c(i1, i2));
        } else {
            ## if (i2 > 10) {
            ##     return(c(ceiling((i1 + 1) * i2 / 10), 10));
            ## } else {
            ##     return(c(i1 + 1), i2);
            ## }
            return(c(i1 + 1, i2));
        }
    } else {
        warning(">> N == 0, returning NULL!");
        return(NULL);
    }
}  ## End of CalculatePlotLayout
################################################################################

################################################################################
updateProgress <- function(progress, amount, detail = NULL) {
    progress$inc(amount = amount, detail = detail);
}  ## End of updateProgress
################################################################################

################################################################################
GetPeak <- function(A, k, theta) {
    return(A * (k - 1) ^ (k - 1) * exp(-(k - 1)) / (gamma(k) * theta));
}  ## End of GetPeak
################################################################################

################################################################################
GetSummary <- function(smry, full = FALSE) {
################################################################################
    x <- capture.output(print(smry));
    ## x <- printed("print(fit$smry)");
    if (full) {
        return(x);
    } else {
        x <- x[x != ""];
        ## return(c(x[((1:length(x))[x == "Parameters:"]):((1:length(x))[substr(x, 0, 3) == "---"] - 1)],
        ##          x[substr(x, 0, 8) == "Residual"]));
        x <- x[substr(x, 0, 7) != "Signif."];
        y <- c(x[((1:length(x))[x == "Parameters:"]):((1:length(x))[substr(x, 0, 8) == "Residual"])]);
        y[length(y)] <- paste(substr(y[length(y)], 0, nchar(y[length(y)]) - 19), "d.o.f.");
        return(y);
    }
################################################################################
}  ## End of GetSummary()
################################################################################

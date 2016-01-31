################################################################################
GetDrv1 <- function(env, n = 3) {  ## 2, 3, 5, 10
    if (!is.null(env$data.tg)) {
        N <- length(env$data.tg[["x"]]);
        dt <- env$data.tg[["x"]][2] - env$data.tg[["x"]][1];
        env$data.tg.drv1 <- rep(NA);
        env$data.tg.drv1[1:(N - 3)] = (1 / (4 * dt)) * (
            -env$data.tg[["y"]][1:(N - 3)] -env$data.tg[["y"]][2:(N - 2)] +
                env$data.tg[["y"]][3:(N - 1)] + env$data.tg[["y"]][4:N]);
        ## i <- 1:(N - n);
        ## if (n == 2) {
        ##     e$data.tg.drv1[i] <- (1 / (2 * dt)) * (-e$data.tg[["y"]][i] + e$data.tg[["y"]][i + 2]);
        ## } else if (n == 3) {
        ##     e$data.tg.drv1[i] <- (1 / (4 * dt)) * (-e$data.tg[["y"]][i] -e$data.tg[["y"]][i + 1] + e$data.tg[["y"]][i + 2] + e$data.tg[["y"]][i + 3]);
        ## } else if (n == 5) {
        ##     e$data.tg.drv1[i] <- (1 / (16 * dt)) * (-e$data.tg[["y"]][i] -3 * e$data.tg[["y"]][i + 1] -2 * e$data.tg[["y"]][i + 2] +
        ##                                       2 * e$data.tg[["y"]][i + 3] + 3 * e$data.tg[["y"]][i + 4] + e$data.tg[["y"]][i + 5]);
        ## } else if (n == 10) {
        ##     e$data.tg.drv1[i] <- (1 / (512 * dt)) * (-e$data.tg[["y"]][i] -8 * e$data.tg[["y"]][i + 1] -27 * e$data.tg[["y"]][i + 2] -
        ##                                        48 * e$data.tg[["y"]][i + 3] -42 * e$data.tg[["y"]][i + 4] + 0 * e$data.tg[["y"]][i + 5] +
        ##                                            42 * e$data.tg[["y"]][i + 6] + 48 * e$data.tg[["y"]][i + 7] + 27 * e$data.tg[["y"]][i + 8] +
        ##                                                8 * e$data.tg[["y"]][i + 9] + e$data.tg[["y"]][i + 10]);
        ## }
    } else {
        print(">> GetDrv1: env$data.tg == NULL!");
    }
}  ## End of GetDrv1
################################################################################

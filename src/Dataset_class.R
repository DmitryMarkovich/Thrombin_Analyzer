################################################################################
Dataset.clear <- function() {
    data <<- data.frame();
    res <<- list();
}  ## End of Dataset.clear
################################################################################

################################################################################
Dataset.load_signal <- function(inFile) {
    print(">> Dataset.load_signal called!");
    ## print(sub(pattern = ".*[.]", "", inFile$name));
    switch(sub(pattern = ".*[.]", "", inFile$name),
           "csv" = {
               data <<- read.csv(file = inFile$datapath, header = TRUE,
                                 sep = ";");
           }
           )
    ## remove all rows containing NA's
    data <<- data[complete.cases(data), ];
    ## replaces all spaces with underscores in column names
    colnames(data) <<- gsub("[.]", "_", colnames(data));
    N <<- length(data);
    signals <<- colnames(data);
}  ## End of Dataset.load_signal
################################################################################

################################################################################
Dataset.plot <- function(updateProgress = NULL, progress) {
    if (length(data) != 0) {
        ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
        ## par(mar = c(4, 7, 2, 0.5), mgp = c(10, 1, 0));
        ## N <- length(data); titles <- colnames(data);
        plot.matrix <- CalculatePlotLayout(N - 1); print(plot.matrix);
        par(mfrow = plot.matrix, mar = rep(0, 4));  ## options(scipen = -2); pin = c(0.5, 0.5)
        for (i in 2:N) {
            ## Sys.sleep(0.1);
            ## print(paste0(">> Plotting ", titles[i]));
            graphics::plot(data[[1]], data[[i]], main = NA, xlab = NA, ylab = NA,
                           cex = 0.5, cex.axis = 0.5, cex.main = 0.5, axes = FALSE,
                           ylim = c(0, max(data[[i]], na.rm = TRUE)));
            box();
            title(i - 1, line = -1.5, cex.main = 1.5);
            ## If we were passed a progress update function, call it
            if (is.function(updateProgress)) {
                text <- paste0(", compound ", i - 1, " out of ", N - 1);
                updateProgress(progress, amount = 1 / (N - 1), detail = text);
            }
        }
    } else {
        warning(">> length(data) == 0, returning NULL!");
        return(NULL);
    }
}  ## End of Dataset.plot
################################################################################

################################################################################
Dataset.do_analysis <- function(updateProgress = NULL, progress) {
    if (!is.null(data) && length(data) != 0) {
        print(">> DoAnaysis called!");
        time <- data[, 1];  ## print(time);
        ## signals <- colnames(data);  ## print(signals);
        for (i in 2:N) {
            print(paste0(">> Processing ", signals[i]));
            tmp <- TG$new();  ## str(tmp);
            tmp$data <- data.frame(x = time, y = data[, signals[i]]);
            tmp$explore_numerically();
            tmp$fit_Auto();
            res[[signals[i]]] <<- list(num.smry = tmp$num.smry,
                                       Auto_model = tmp$fit$Auto_model,
                                       Auto_fit = tmp$fit);
            ## str(tmp);
            if (is.function(updateProgress)) {
                text <- paste0("compound ", i - 1, " out of ", N - 1);
                updateProgress(progress, amount = 1 / (N - 1), detail = text);
            }
        }
        print(res);
    } else {
        warning(">> data is NULL or empty!");
        return(NULL);
    }
}  ## End of Dataset.do_analysis
################################################################################

################################################################################
Dataset <- setRefClass(
    Class = "Dataset",
    fields = list(data = "data.frame", N = "integer", signals = "vector",
        res = "list"),
    methods = list(
        clear = Dataset.clear,
        load_signal = Dataset.load_signal,
        plot = Dataset.plot,
        do_analysis = Dataset.do_analysis
        ## explore_numerically = Dataset.explore_numerically,
        ## conv_pvals_to_signif_codes = Dataset.ConvPValsToSignifCodes
        )
    );  ## End of Dataset
################################################################################

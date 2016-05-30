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
source("src/Dataset_plotting_methods.R");

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
            tmp$explore_numerically(); tmp$evaluate_numerically();
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
        plot = Dataset.plot, plot_overlay = Dataset.plot_overlay,
        plot_drv1_overlay = Dataset.plot_drv1_overlay,
        plot_drv2_overlay = Dataset.plot_drv2_overlay,
        do_analysis = Dataset.do_analysis
        ## explore_numerically = Dataset.explore_numerically,
        ## conv_pvals_to_signif_codes = Dataset.ConvPValsToSignifCodes
        )
    );  ## End of Dataset
################################################################################

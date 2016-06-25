################################################################################
Dataset.is_empty <- function() {
    if (length(data) == 0 && length(N) == 0 && length(signals) == 0 &&
        length(parms) == 0) {
        return(TRUE);
    } else {
        return(FALSE);
    }
}  ## End of Dataset.is_empty
################################################################################

################################################################################
Dataset.clear <- function() {
    print(">> Dataset.clear called!");
    data <<- data.frame(); N <<- 0L; signals <<- vector(mode = "character");
    res <<- list(); parms <<- data.frame();
    return(0L);
}  ## End of Dataset.clear
################################################################################

################################################################################
Dataset.load <- function(inFile) {
    print(">> Dataset.load called!");
    ## print(sub(pattern = ".*[.]", "", inFile$name));
    switch(sub(pattern = ".*[.]", "", inFile$name),
           "csv" = {
               data <<- read.csv(file = inFile$datapath, header = TRUE,
                                 sep = ";");
           }
           )
    ## remove all rows containing NA's
    data <<- data[complete.cases(data), ];
    ## replace all spaces with underscores in column names
    signals <<- gsub("[.]", "_", colnames(data));
    colnames(data) <<- signals; N <<- length(data);
    return(0L);
}  ## End of Dataset.load
################################################################################

################################################################################
Dataset.load_results <- function(inFile) {
    ## print(">> Dataset.load_results called!");
    ## print(sub(pattern = ".*[.]", "", inFile$name));
    switch(sub(pattern = ".*[.]", "", inFile$name),
           "RData" = {
               res <<- LoadRData(fname = inFile$datapath);  ## print(res);
           }
           );
    return(0L);
}  ## End of Dataset.load_results
################################################################################
source("src/Dataset_plotting_methods.R");

################################################################################
Dataset.copy_and_analyze_TG <- function(x = 0, y = 0, expl_num = TRUE,
                                        eval_num = TRUE, fit_Auto = TRUE,
                                        signal = NULL) {
    tmp <- TG$new();  ## str(tmp);
    tmp$data <- data.frame(x = x, y = y);
    if (is.null(signal)) {
        if (expl_num)
            tmp$explore_numerically();
        if (eval_num)
            tmp$evaluate_numerically();
        if (fit_Auto)
            tmp$fit_Auto();
    } else {
        if (any(signal == signals)) {
            if (exists(x = signal, where = res)) {
                tmp$num.smry <- res[[signal]]$num.smry;
                tmp$num.eval <- res[[signal]]$num.eval;
                tmp$fit <- res[[signal]]$Auto_fit;
            }
        }
    }
    return(tmp);
}  ## End of Dataset.copy_and_analyze_TG
################################################################################

Dataset.do_analysis_in_loop <- function(i) {
    ## print(paste0(">> Processing ", signals[i]));
    tmp <- copy_and_analyze_TG(x = time, y = data[, signals[i]]);
    res[[signals[i]]] <<- list(num.smry = tmp$num.smry,
                               num.eval = tmp$num.eval,
                               Auto_model = tmp$fit$Auto_model,
                               Auto_fit = tmp$fit,
                               parms = tmp$parms_model(tmp$fit$Auto_model));
    if (!any(tmp$fit$Auto_model == c("None", "T0Gamma"))) {
        parms[i - 1, 2:(2 + length(kParameterNames))] <<- list(
            Reliable = "Yes",
            tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
            tmp$parms$Value[tmp$parms$Parameter == "ETP"],
            tmp$parms$Value[tmp$parms$Parameter == "Peak"],
            tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
            tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
            tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
            );
    } else {
        parms[i - 1, 3:(2 + length(kParameterNames))] <<- list(
            tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
            tmp$parms$Value[tmp$parms$Parameter == "ETP"],
            tmp$parms$Value[tmp$parms$Parameter == "Peak"],
            tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
            tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
            tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
            );
    }
    if (is.function(updateProgress)) {
        text <- paste0(" compound ", i - 1, " out of ", N - 1);
        updateProgress(progress, amount = 1 / (N - 1), detail = text);
    }
}


################################################################################
Dataset.do_analysis <- compiler::cmpfun(
    f = function(updateProgress = NULL, progress) {
    if (!is.null(data) && length(data) != 0) {
        print(">> DoAnalysis called!");
        time <- data[, 1];  ## print(time);
        parms <<- data.frame(
            Signal = signals[-1], Reliable = rep("No", N - 1),
            Lagtime = rep(NA, N - 1), ETP = rep(NA, N - 1),
            Peak = rep(NA, N - 1), ttPeak = rep(NA, N - 1),
            VelIndex = rep(NA, N - 1), Alpha2M_Level = rep(NA, N - 1),
            stringsAsFactors = FALSE);
        if (length(res) != 0) {
            ## res is currently loaded, but now has to be erased
            ## res <<- list();
            res <<- vector(mode = "list", length = N - 1);
            names(res) <<- signals[-1];
        }
        start.time <- proc.time();
        ## for (i in 2:N) {
        ##     ## print(paste0(">> Processing ", signals[i]));
        ##     tmp <- copy_and_analyze_TG(x = time, y = data[, signals[i]]);
        ##     res[[signals[i]]] <<- list(num.smry = tmp$num.smry,
        ##                                num.eval = tmp$num.eval,
        ##                                Auto_model = tmp$fit$Auto_model,
        ##                                Auto_fit = tmp$fit,
        ##                                parms = tmp$parms_model(tmp$fit$Auto_model));
        ##     if (!any(tmp$fit$Auto_model == c("None", "T0Gamma"))) {
        ##         parms[i - 1, 2:(2 + length(kParameterNames))] <<- list(
        ##             Reliable = "Yes",
        ##             tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "ETP"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "Peak"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
        ##             );
        ##     } else {
        ##         parms[i - 1, 3:(2 + length(kParameterNames))] <<- list(
        ##             tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "ETP"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "Peak"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
        ##             tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
        ##             );
        ##     }
        ##     if (is.function(updateProgress)) {
        ##         text <- paste0(" compound ", i - 1, " out of ", N - 1);
        ##         updateProgress(progress, amount = 1 / (N - 1), detail = text);
        ##     }
        ## }  ## End of for i
        ## lapply(X = 2:N, FUN = function(i) {
        ##            ## print(paste0(">> Processing ", signals[i]));
        ##            tmp <- copy_and_analyze_TG(x = time, y = data[, signals[i]]);
        ##                res[[signals[i]]] <<- list(num.smry = tmp$num.smry,
        ##                                           num.eval = tmp$num.eval,
        ##                                           Auto_model = tmp$fit$Auto_model,
        ##                                           Auto_fit = tmp$fit,
        ##                                           parms = tmp$parms_model(tmp$fit$Auto_model));
        ##            if (!any(tmp$fit$Auto_model == c("None", "T0Gamma"))) {
        ##                parms[i - 1, 2:(2 + length(kParameterNames))] <<- list(
        ##                    Reliable = "Yes",
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "ETP"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Peak"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
        ##                    );
        ##            } else {
        ##                parms[i - 1, 3:(2 + length(kParameterNames))] <<- list(
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Lagtime"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "ETP"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Peak"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "ttPeak"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "VelIndex"],
        ##                    tmp$parms$Value[tmp$parms$Parameter == "Alpha2M_Level"]
        ##                    );
        ##            }

        ##        });
        ## cores <- ;
        tmp.res <- parallel::mclapply(
            X = 2:N, FUN = function(i) {
                ## ## local copy of empty res
                ## res <- vector(mode = "list", length = N - 1);
                ## names(res) <<- signals[-1];
                ## ## local copy of empty parms
                ## parms <- data.frame(
                ##     Signal = signals[-1], Reliable = rep("No", N - 1),
                ##     Lagtime = rep(NA, N - 1), ETP = rep(NA, N - 1),
                ##     Peak = rep(NA, N - 1), ttPeak = rep(NA, N - 1),
                ##     VelIndex = rep(NA, N - 1), Alpha2M_Level = rep(NA, N - 1),
                ##     stringsAsFactors = FALSE);
                ## print(paste0(">> Processing ", signals[i]));
                ##            if (is.function(updateProgress)) {
                ##                text <- paste0(" compound ", i - 1, " out of ", N - 1);
                ##                updateProgress(progress, amount = 1 / (N - 1), detail = text);
                ##            }
                return(copy_and_analyze_TG(x = time, y = data[, signals[i]]));
                ## return(list(res = res, parms = parms));
            }, mc.cores = parallel::detectCores()
            );
        stop.time <- proc.time() - start.time;
        print(paste0(">> main loop took [s]"));
        print(stop.time);
        ## print(tmp.res);
        for (i in 2:N) {
            res[[signals[i]]] <<- list(
                num.smry = tmp.res[[i - 1]]$num.smry,
                num.eval = tmp.res[[i - 1]]$num.eval,
                Auto_model = tmp.res[[i - 1]]$fit$Auto_model,
                Auto_fit = tmp.res[[i - 1]]$fit,
                parms = tmp.res[[i - 1]]$parms_model(tmp.res[[i - 1]]$fit$Auto_model)
                );
            ## print(paste0(">> res from i = ", i));
            ## print(res);
            if (!any(tmp.res[[i - 1]]$fit$Auto_model == c("None", "T0Gamma"))) {
                parms[i - 1, 2:(2 + length(kParameterNames))] <<- list(
                    Reliable = "Yes",
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Lagtime"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "ETP"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Peak"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "ttPeak"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "VelIndex"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Alpha2M_Level"]
                    );
            } else {
                parms[i - 1, 3:(2 + length(kParameterNames))] <<- list(
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Lagtime"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "ETP"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Peak"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "ttPeak"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "VelIndex"],
                    tmp.res[[i - 1]]$parms$Value[tmp.res[[i - 1]]$parms$Parameter == "Alpha2M_Level"]
                    );
            }
            ## if (is.function(updateProgress)) {
            ##     text <- paste0(" compound ", i - 1, " out of ", N - 1);
            ##     updateProgress(progress, amount = 1 / (N - 1), detail = text);
            ## }
        }  ## End of for i
        ## print(tmp.res);
        ## res <<- tmp.res$res; parms <<- tmp.res$parms;
        ## print(res); ## print(parms);
        return(0L);
    } else {
        warning(">> data is NULL or empty!");
        return(NULL);
    }
}, options = kCmpFunOptions);  ## End of Dataset.do_analysis
################################################################################

################################################################################
Dataset <- setRefClass(
    Class = "Dataset",
    fields = list(data = "data.frame", N = "integer", signals = "vector",
        res = "list", parms = "data.frame"),
    methods = list(
        is_empty = Dataset.is_empty, clear = Dataset.clear,
        load = Dataset.load, load_results = Dataset.load_results,
        copy_and_analyze_TG = Dataset.copy_and_analyze_TG,
        plot = Dataset.plot, plot_overlay = Dataset.plot_overlay,
        plot_drv1_overlay = Dataset.plot_drv1_overlay,
        plot_drv2_overlay = Dataset.plot_drv2_overlay,
        do_analysis_in_loop = Dataset.do_analysis_in_loop,
        do_analysis = Dataset.do_analysis
        ## explore_numerically = Dataset.explore_numerically,
        ## conv_pvals_to_signif_codes = Dataset.ConvPValsToSignifCodes
        )
    );  ## End of Dataset
################################################################################
## print(Dataset);

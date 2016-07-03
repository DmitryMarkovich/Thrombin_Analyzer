################################################################################
Dataset <- R6::R6Class(
    classname = "Dataset", portable = FALSE,
    private = list(
        data = "data.frame", N = "integer", signals = "vector",
        res = "list", parms = "data.frame"
        ),
    public = list(
        initialize = compiler::cmpfun(
            f = function() {
                data <<- NULL;
                N <<- NULL;
                signals <<- NULL;
                res <<- NULL;
                parms <<- NULL;
            }, options = kCmpFunOptions),
        is_empty = compiler::cmpfun(
            f = function() {
                if (length(data) == 0 && length(N) == 0 && length(signals) == 0 &&
                    length(parms) == 0) {
                    return(TRUE);
                } else {
                    return(FALSE);
                }
            }, options = kCmpFunOptions),
        clear = compiler::cmpfun(
            f = function() {
                print(">> Dataset.clear called!");
                data <<- data.frame(); N <<- 0L; signals <<- vector(mode = "character");
                res <<- list(); parms <<- data.frame();
                return(0L);
            }, options = kCmpFunOptions),
        load  = compiler::cmpfun(
            f = function(inFile) {
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
                ## print(object.size(data) / 1e6);
                return(0L);
            }, options = kCmpFunOptions),
        load_RData = compiler::cmpfun(
            f = function(fname = NULL) {
                if (!is.null(fname)) {
                    ## print(ls());
                    base::load(fname);
                    ## print(ls());
                    return(get(x = ls()[ls() != "fname"]));
                } else {
                    warning(">> NULL name in load_RData!");
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        load_results = compiler::cmpfun(
            f = function(inFile) {
                ## print(">> Dataset.load_results called!");
                ## print(sub(pattern = ".*[.]", "", inFile$name));
                switch(sub(pattern = ".*[.]", "", inFile$name),
                       "RData" = {
                           res <<- load_RData(fname = inFile$datapath);
                       }
                       );
                return(0L);
            }, options = kCmpFunOptions),  ## End of Dataset.load_results
        updateProgress = function(progress, amount, detail = NULL) {
            progress$inc(amount = amount, detail = detail);
        },
        load_parms = compiler::cmpfun(
            f = function(inFile) {
                switch(sub(pattern = ".*[.]", "", inFile$name),
                       "csv" = {
                           parms <<- read.csv(file = inFile$datapath,
                                              header = TRUE, sep = ";");
                       }
                       );
                return(0L);
            }, options = kCmpFunOptions),
        get_data = compiler::cmpfun(
            f = function() {
                return(data);
            }, options = kCmpFunOptions),
        get_time = compiler::cmpfun(
            f = function() {
                return(data[, 1]);
            }, options = kCmpFunOptions),
        get_data_column = compiler::cmpfun(
            f = function(colname) {
                if (any(colname == signals)) {
                    return(data[[colname]]);
                } else {
                    print(">> Unknown column selected!");
                    return(rep(NA, N));
                }
            }, options = kCmpFunOptions),
        get_N = compiler::cmpfun(
            f = function() {
                return(N);
            }, options = kCmpFunOptions),
        get_signals = compiler::cmpfun(
            f = function() {
                return(signals);
            }, options = kCmpFunOptions),
        get_parms = compiler::cmpfun(
            f = function() {
                return(parms);
            }, options = kCmpFunOptions),
        set_parms = compiler::cmpfun(
            f = function() {
                parms <<- data.frame(
                    Signal = signals[-1], Reliable = rep(FALSE, N - 1),
                    Lagtime = rep(NA_real_, N - 1), ETP = rep(NA_real_, N - 1),
                    Peak = rep(NA_real_, N - 1), ttPeak = rep(NA_real_, N - 1),
                    VelIndex = rep(NA_real_, N - 1), Alpha2M_Level = rep(NA_real_, N - 1),
                    stringsAsFactors = FALSE);
                return(0L);
            }, options = kCmpFunOptions),
        get_res = compiler::cmpfun(
            f = function() {
                return(res);
            }, options = kCmpFunOptions),
        set_res = compiler::cmpfun(
            f = function() {
                if (length(res) != 0) {
                    ## res is currently loaded, but now has to be erased
                    ## res <<- list();
                    res <<- vector(mode = "list", length = N - 1);
                    names(res) <<- signals[-1];
                }
            }, options = kCmpFunOptions),
        get_res_length = compiler::cmpfun(
            f = function() {
                return(length(res));
            }, options = kCmpFunOptions),
        res_length_ok = compiler::cmpfun(
            f = function() {
                return(length(res) == N - 1);
            }, options = kCmpFunOptions),
        get_Auto_fit = compiler::cmpfun(
            f = function(i = 1) {
                return(res[[i]]$Auto_fit);
            }, options = kCmpFunOptions),
        get_Auto_model = compiler::cmpfun(
            f = function(i = 1) {
                return(res[[i]]$Auto_model);
            }, options = kCmpFunOptions),
        get_num_eval = compiler::cmpfun(
            f = function(i = 1) {
                return(res[[i]]$num.eval);
            }, options = kCmpFunOptions),
        overlay_parms = compiler::cmpfun(
            f = function(signal1, signal2) {
                if (any(signal1 == signals) && any(signal2 == signals)) {
                    return(parms[parms$Signal == signal1 |
                                     parms$Signal == signal2, ]);
                ## rbin(Units = c(NA, NA, kAUnits))
                } else {
                    return(NULL);
                }
            }, options = kCmpFunOptions)
        )  ## End of public
    );  ## End of Dataset
################################################################################

source("src/Dataset_plotting_methods.R");
source("src/Dataset_analysis_methods.R");
## print(Dataset);

## ################################################################################
## ######################################## Legacy RF classes code
## ################################################################################

## ################################################################################
## Dataset.is_empty <- function() {
##     if (length(data) == 0 && length(N) == 0 && length(signals) == 0 &&
##         length(parms) == 0) {
##         return(TRUE);
##     } else {
##         return(FALSE);
##     }
## }  ## End of Dataset.is_empty
## ################################################################################

## ################################################################################
## Dataset.clear <- function() {
##     print(">> Dataset.clear called!");
##     data <<- data.frame(); N <<- 0L; signals <<- vector(mode = "character");
##     res <<- list(); parms <<- data.frame();
##     return(0L);
## }  ## End of Dataset.clear
## ################################################################################

## ################################################################################
## Dataset.load <- function(inFile) {
##     print(">> Dataset.load called!");
##     ## print(sub(pattern = ".*[.]", "", inFile$name));
##     switch(sub(pattern = ".*[.]", "", inFile$name),
##            "csv" = {
##                data <<- read.csv(file = inFile$datapath, header = TRUE,
##                                  sep = ";");
##            }
##            )
##     ## remove all rows containing NA's
##     data <<- data[complete.cases(data), ];
##     ## replace all spaces with underscores in column names
##     signals <<- gsub("[.]", "_", colnames(data));
##     colnames(data) <<- signals; N <<- length(data);
##     return(0L);
## }  ## End of Dataset.load
## ################################################################################

## ################################################################################
## Dataset.load_results <- function(inFile) {
##     ## print(">> Dataset.load_results called!");
##     ## print(sub(pattern = ".*[.]", "", inFile$name));
##     switch(sub(pattern = ".*[.]", "", inFile$name),
##            "RData" = {
##                res <<- LoadRData(fname = inFile$datapath);  ## print(res);
##            }
##            );
##     return(0L);
## }  ## End of Dataset.load_results
## ################################################################################

## ################################################################################
## Dataset <- setRefClass(
##     Class = "Dataset",
##     fields = list(data = "data.frame", N = "integer", signals = "vector",
##         res = "list", parms = "data.frame"),
##     methods = list(
##         is_empty = Dataset.is_empty, clear = Dataset.clear,
##         load = Dataset.load, load_results = Dataset.load_results,
##         copy_and_analyze_TG = Dataset.copy_and_analyze_TG,
##         plot = Dataset.plot, plot_overlay = Dataset.plot_overlay,
##         plot_drv1_overlay = Dataset.plot_drv1_overlay,
##         plot_drv2_overlay = Dataset.plot_drv2_overlay,
##         do_analysis_in_loop = Dataset.do_analysis_in_loop,
##         do_analysis = Dataset.do_analysis
##         ## explore_numerically = Dataset.explore_numerically,
##         ## conv_pvals_to_signif_codes = Dataset.ConvPValsToSignifCodes
##         )
##     );  ## End of Dataset
## ################################################################################
## ## print(Dataset);

## ################################################################################
## ######################################## End of Legacy RF classes code
## ################################################################################

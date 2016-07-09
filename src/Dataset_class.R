################################################################################
Dataset <- R6::R6Class(
    classname = "Dataset", portable = FALSE,
    private = list(
        data = data.frame(), N = integer(0),
        signals = vector(mode = "character", length = 0),
        res = list(), parms = data.frame(),
        tg1 = TG$new(), tg2 = TG$new()
        ),
    public = list(
        initialize = compiler::cmpfun(
            f = function() {
                data <<- NULL;
                N <<- NULL;
                signals <<- NULL;
                res <<- NULL;
                parms <<- NULL;
                tg1 = TG$new(); tg2 = TG$new();
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
                data <<- data.frame(); N <<- integer(0);
                signals <<- vector(mode = "character", length = 0);
                res <<- list(); parms <<- data.frame();
                tg1 = TG$new(); tg2 = TG$new();
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
            f = function(na.rm = FALSE) {
                ## print(cbind(parms, Num = 1:length(parms$Signal)));
                if (na.rm) {
                    if (is.null(parms$Num)) {
                        tmp <- cbind(parms, Num = 1:length(parms$Signal));
                        return(tmp[complete.cases(tmp), ]);
                    } else {
                        return(parms[complete.cases(parms), ]);
                    }
                } else {
                    if (is.null(parms$Num)) {
                        return(cbind(parms, Num = 1:length(parms$Signal)));
                    } else {
                        return(parms);
                    }
                }
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
        get_tg = compiler::cmpfun(
            f = function(which = 1) {
                if (any(which == c(1, 2))) {
                    return(get(paste0(x = "tg", which)));
                } else {
                    print(">> Unknown tg object to get!");
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        set_tg = compiler::cmpfun(
            f = function(input, which = 1) {
                if (any(which == c(1, 2))) {
                    if (which == 1) {
                        tg1 <<- copy_and_analyze_TG(x = data[[1]],
                                                    y = data[[input]],
                                                    signal = input);
                        if (!tg1$is_ok_num_smry())
                            tg1$explore_numerically();
                    }
                    if (which == 2) {
                        tg2 <<- copy_and_analyze_TG(x = data[[1]],
                                                    y = data[[input]],
                                                    signal = input);
                        if (!tg2$is_ok_num_smry())
                            tg2$explore_numerically();
                    }
                    ## assign(x = paste0("tg", which),
                    ##        value = copy_and_analyze_TG(x = data[[1]],
                    ##            y = data[[input]], signal = input),
                    ##        pos = self);
                    return(0L);
                } else {
                    print(">> Unknown tg object to set!");
                    return(NULL);
                }
            }, options = kCmpFunOptions),
        print_tg = compiler::cmpfun(
            f = function(which = 1) {
                if (any(which == c(1, 2))) {
                    print(head(get(x = paste0("tg", which))$get_data()));
                    print(head(get(x = paste0("tg", which))$get_num_smry(), n = 1));
                    print(get(x = paste0("tg", which))$get_fit());
                } else {
                    print(">> Unknown tg object to print!");
                    return(NULL);
                }
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
            }, options = kCmpFunOptions),
        get_ylim_overlay = compiler::cmpfun(
            f = function(y1, y2) {
                return(c(
                    min(c(
                        min(y1, na.rm = TRUE),
                        min(y2, na.rm = TRUE)
                        ), na.rm = TRUE),
                    max(c(
                        max(y1, na.rm = TRUE),
                        max(y2, na.rm = TRUE)
                        ), na.rm = TRUE)
                    ));
            }, options = kCmpFunOptions)
        )  ## End of public
    );  ## End of Dataset
################################################################################

source("src/Dataset_plotting_methods.R");
source("src/Dataset_analysis_methods.R");

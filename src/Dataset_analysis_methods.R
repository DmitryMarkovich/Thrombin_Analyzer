################################################################################
Dataset$set(
    which = "public", name = "copy_and_analyze_TG",
    value = compiler::cmpfun(
        f = function(x = 0, y = 0, expl_num = TRUE, eval_num = TRUE,
            fit_Auto = TRUE, signal = NULL, copy.res = TRUE,
                     updateProgress = NULL, progress = NULL) {
            tmp <- TG$new();  ## str(tmp);
            tmp$clear();
            ## tmp$data <- data.frame(x = x, y = y);
            ## print("x = "); print(x);
            ## print("y = "); print(y);
            tmp$set_data(data.frame(x = x, y = y));
            ## print("tmp$data = "); print(tmp$get_data());
            if (is.null(signal)) {
                ## if (expl_num)
                tmp$explore_numerically();
                ## if (eval_num)
                tmp$evaluate_numerically();
                ## if (fit_Auto)
                tmp$fit_Auto();
                tmp$parms_model(tmp$auto_model());
            } else {
                if (any(signal == signals)) {
                    if (copy.res && !is.null(res[[signal]])) {
                        ## print(res[[signal]]$num.smry);
                        tmp$set_num_smry(res[[signal]]$num.smry);
                        tmp$set_num_eval(res[[signal]]$num.eval);
                        tmp$set_fit(res[[signal]]$Auto_fit);
                        ## tmp$num.smry <- res[[signal]]$num.smry;
                        ## tmp$num.eval <- res[[signal]]$num.eval;
                        ## tmp$fit <- res[[signal]]$Auto_fit;
                    }
                }
            }
            ## print(tmp$get_parms());
            if (!is.null(updateProgress) && !is.null(progress)) {
                if (is.function(updateProgress)) {
                    ## text <- paste0(", compound ", i - 1, " out of ", N - 1);
                    text <- "";
                    updateProgress(progress, amount = 1 / (N - 1), detail = text);
                }
            }
            return(tmp);
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$copy_and_analyze_TG
################################################################################

################################################################################
Dataset$set(
    which = "public", name = "do_analysis",
    value =  compiler::cmpfun(
        f = function(updateProgress = NULL, progress = NULL) {
            if (!is.null(data) && length(data) != 0) {
                print(">> DoAnalysis called!");
                time <- data[, 1];  ## print(time);
                set_parms(); set_res();
                start.time <- proc.time();
                tmp.res <- parallel::mclapply(
                    X = 2:N, FUN = function(i) {
                        return(copy_and_analyze_TG(
                            x = time, y = data[, signals[i]],
                            updateProgress = updateProgress, progress = progress
                            ));
                    }, mc.cores =  parallel::detectCores()
                    );
                stop.time <- proc.time() - start.time;
                print(paste0(">> main loop took [s]"));
                print(stop.time);
                ## print(tmp.res[[1]]);
                print(object.size(tmp.res) / 1e6);
                ## print(tmp.res);
                start.time <- proc.time();
                ## print(paste0(">> tmp.res size = ", object.size(tmp.res) / 1e6, " MB"));
                for (i in 2:N) {
                    res[[signals[i]]] <<- list(
                        ## num.smry = tmp.res[[i - 1]]$get_num_smry(),
                        ## num.eval = tmp.res[[i - 1]]$get_num_eval(),
                        Auto_model = tmp.res[[i - 1]]$auto_model(),
                        Auto_fit = tmp.res[[i - 1]]$get_fit()
                        );
                    ## ,
                    ## parms = tmp.res[[i - 1]]$parms_model(tmp.res[[i - 1]]$auto_model())
                    if (!any(tmp.res[[i - 1]]$auto_model() == c("None", "T0Gamma"))) {
                        parms[i - 1, 2:(2 + 1 + length(kParameterNames))] <<- list(
                            Reliable = TRUE,
                            Model = tmp.res[[i - 1]]$auto_model(),
                            tmp.res[[i - 1]]$get_parameter("Lagtime"),
                            tmp.res[[i - 1]]$get_parameter("ETP"),
                            tmp.res[[i - 1]]$get_parameter("Peak"),
                            tmp.res[[i - 1]]$get_parameter("ttPeak"),
                            tmp.res[[i - 1]]$get_parameter("VelIndex"),
                            tmp.res[[i - 1]]$get_parameter("Alpha2M_Level")
                            );
                    } else {
                        parms[i - 1, 3:(2 + 1 + length(kParameterNames))] <<- list(
                            Model = tmp.res[[i - 1]]$auto_model(),
                            tmp.res[[i - 1]]$get_parameter("Lagtime"),
                            tmp.res[[i - 1]]$get_parameter("ETP"),
                            tmp.res[[i - 1]]$get_parameter("Peak"),
                            tmp.res[[i - 1]]$get_parameter("ttPeak"),
                            tmp.res[[i - 1]]$get_parameter("VelIndex"),
                            tmp.res[[i - 1]]$get_parameter("Alpha2M_Level")
                            );
                    }
                }  ## End of for i
                stop.time <- proc.time() - start.time;
                print(paste0(">> copy loop took [s]"));
                print(stop.time);
                print(paste0(">> data is ", object.size(data) / 1e6, " MB"));
                print(paste0(">> res is ", object.size(res) / 1e6, " MB"));
                print(paste0(">> res / data is ", object.size(res) /
                                 object.size(data)));
                ## print(res);  ## print(parms);
                return(0L);
            } else {
                warning(">> data is NULL or empty!");
                return(NULL);
            }
        }, options = kCmpFunOptions),
    overwrite = FALSE);  ## End of Dataset$do_analysis
################################################################################

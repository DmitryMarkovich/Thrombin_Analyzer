N.tries <- 20;  ## attempts to perform fitting
kPlotTypes <- c("Calibration", "Thrombin generation", "Thrombogram");
kDataCodes <- c("cal", "tg", "tg.drv1");
kCalDFPar <- c("e0", "s0", "K.m", "k.cat", "C", "CF_CAT", "CF_DTU");
value <- runif(n = length(kCalDFPar));
stderr <- runif(n = length(kCalDFPar));
kCalDFDim <- c("nM", "nM", "nM", "nM / min", "a.u. / nM", "nM * min / a.u.",
               "nM * min / a.u.");

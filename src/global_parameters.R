N.tries <- 1000;  ## attempts to perform fitting
kParameterNames <- c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                     "Alpha2M_Level");
kAUnits <- c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                   "a.u. / min");
kUnits <- 
kPlotTypes <- c("Calibration", "Thrombin generation", "Thrombogram");
kDataCodes <- c("cal", "tg", "tg.drv1");
kCalDFPar <- c("e0", "s0", "K.m", "k.cat", "C", "CF_CAT", "CF_DTU");
value <- runif(n = length(kCalDFPar));
stderr <- runif(n = length(kCalDFPar));
kCalDFDim <- c("nM", "nM", "nM", "nM / min", "a.u. / nM", "nM * min / a.u.",
               "nM * min / a.u.");

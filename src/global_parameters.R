kNumTries <- 1000;  ## number of attempts to perform fitting
kParameterNamesLM <- c("e0", "s0", "CF_CAT", "TC_Initial_Slope");
kParameterUnitsLM <- c("nM", "uM", "nM * min / a.u.", "a.u. / min");
kParameterNamesLateMM <- c("e0", "s0", "CF_CAT", "CF_DTU", "K.m", "k.cat", "I",
                           "TC_Initial_Slope");
kParameterUnitsLateMM <- c("nM", "uM", "nM * min / a.u.",
                           "nM * min / a.u.", "nM", "nM / min",
                           "a.u. / nM", "a.u. / min");
kParameterNames <- c("Lagtime", "ETP", "Peak", "ttPeak", "VelIndex",
                     "Alpha2M_Level");

kAUnits <- c("min", "a.u.", "a.u. / min", "min", "a.u. / min * min",
                   "a.u. / min");
kUnits <- c("min", "nM * min", "nM", "min", "nM / min", "nM");

kYNone <- 3;  ## cutoff for rat$y - below 3 signal is mostly noise
kXT0Gamma <- 1.6;  ## cutoff for rat$x - below 1.6 is T0Gamma
kXT0GammaInt <- 2.5;  ## cutoff for rat$x - above 2.5 is T0GammaInt
kYT0GammaInt <- 34;  ## cutoff for rat$y - above 34 signal is T0GammaInt

kYlimMultDrv2 <- 1.5;
kSigmaLMRatio <- 3;

## kPlotTypes <- c("Calibration", "Thrombin generation", "Thrombogram");
## kDataCodes <- c("cal", "tg", "tg.drv1");
## kCalDFPar <- c("e0", "s0", "K.m", "k.cat", "C", "CF_CAT", "CF_DTU");
## value <- runif(n = length(kCalDFPar));
## stderr <- runif(n = length(kCalDFPar));
## kCalDFDim <- c("nM", "nM", "nM", "nM / min", "a.u. / nM", "nM * min / a.u.",
##                "nM * min / a.u.");

## additional R libraries
kLibraries <- c("shiny", "minpack.lm", "LambertW", "parallel", "compiler", "R6",
                "Rcpp");
CheckAllLibrariesAvailable <- function(libraries) {
    for (i in 1:length(libraries)) {
        if (!any(libraries[i] == installed.packages())) {
            stop(paste0(">> Library ", libraries[i], " is not installed!"));
        }
    }
}  ## End of CheckAllLibrariesAvailable
CheckAllLibrariesAvailable(kLibraries);


library(shiny);
options(shiny.maxRequestSize = 500 * 1024 ^ 2);  ## max file upload size in B

## library(minpack.lm);
## library(LambertW);
## library(ggplot2);

## compiler::setCompilerOptions("optimize" = 3);
## compiler::setCompilerOptions("suppressAll" = TRUE);
## compiler::setCompilerOptions("suppressUndefined" = TRUE);
## print(compiler::getCompilerOption(name = "optimize"));
options(warn = 0);
kCmpFunOptions <- list("optimize" = 3, "suppressAll" = TRUE,
                       "suppressUndefined" = TRUE);

## Fast C++ versions of R functions
Rcpp::sourceCpp(file = "src/Rcpp_functions.cpp");
## x <- rnorm(100);
## print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
## print(x);
## y <- drv1(x, 1);
## print(y);
